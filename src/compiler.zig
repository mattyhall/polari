const std = @import("std");
const bc = @import("bytecode.zig");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const sema = @import("sema.zig");

/// Compile takes a program and compiles it into bytecode.
pub const Compiler = struct {
    gpa: std.mem.Allocator,
    chunk: bc.Chunk,
    program: *const parser.Program,
    parent: ?*Compiler = null,

    pub fn init(gpa: std.mem.Allocator, program: *const parser.Program) Compiler {
        return .{ .gpa = gpa, .chunk = bc.Chunk.init(gpa), .program = program };
    }

    fn writeConst(self: *Compiler, v: u32) !void {
        if (v > std.math.maxInt(u16)) {
            try self.chunk.writeOp(.rare);
            try self.chunk.writeRateOp(.const32);
            try self.chunk.writeU32(@intCast(u32, v));
        } else if (v > std.math.maxInt(u8)) {
            try self.chunk.writeOp(.rare);
            try self.chunk.writeRateOp(.const16);
            try self.chunk.writeU16(@intCast(u16, v));
        } else {
            try self.chunk.writeOp(.const8);
            try self.chunk.writeU8(@intCast(u8, v));
        }
    }

    fn writeLocal(self: *Compiler, i: enum { get, geta, set }, v: u32) !void {
        if (v > std.math.maxInt(u16)) {
            try self.chunk.writeOp(.rare);
            try self.chunk.writeRateOp(switch (i) {
                .get => .get32,
                .geta => .geta32,
                .set => .set32,
            });
            try self.chunk.writeU32(@intCast(u32, v));
        } else if (v > std.math.maxInt(u8)) {
            try self.chunk.writeOp(.rare);
            try self.chunk.writeRateOp(switch (i) {
                .get => .get16,
                .geta => .geta16,
                .set => .set16,
            });
            try self.chunk.writeU16(@intCast(u16, v));
        } else {
            try self.chunk.writeOp(switch (i) {
                .get => .get8,
                .geta => .geta8,
                .set => .set8,
            });
            try self.chunk.writeU8(@intCast(u8, v));
        }
    }

    /// getLocalGlobal tries to find the local of the identifier by looking first in the current chunk's set of locals
    /// and then finding the top level compiler and looking for it there otherwise.
    fn getLocalOrGlobal(self: *Compiler, ident: []const u8) !void {
        if (self.chunk.getLocal(ident)) |l| return try self.writeLocal(.get, l);

        var last_parent = self;
        var parent = self.parent;
        while (parent) |p| {
            last_parent = p;
            parent = p.parent;
        }

        const local = last_parent.chunk.getLocal(ident) orelse return error.LocalNotFound;
        try self.writeLocal(.geta, local);
    }

    fn compileExpression(self: *Compiler, expr: *const parser.Expression) !void {
        switch (expr.*) {
            .integer => |i| if (i == 1 or i == -1)
                try self.chunk.writeOp(if (i == 1) .one else .neg_one)
            else
                try self.writeConst(try self.chunk.addConstant(.{ .integer = i })),
            .boolean => |b| try self.chunk.writeOp(if (b) .true else .false),
            .identifier => |n| try self.getLocalOrGlobal(n),
            .binop => |binop| {
                try self.compileExpression(binop.lhs.inner);
                try self.compileExpression(binop.rhs.inner);
                try self.chunk.writeOp(switch (binop.op) {
                    .plus => .add,
                    .minus => .subtract,
                    .multiply => .multiply,
                    .divide => .divide,
                    .apply => unreachable,
                    .eq => .eq,
                    .neq => .neq,
                    .lt => .lt,
                    .lte => .lte,
                    .gt => .gt,
                    .gte => .gte,
                });
            },
            .unaryop => |unaryop| {
                if (unaryop.op == .negate and unaryop.e.inner.* == .integer and unaryop.e.inner.integer == 1) {
                    try self.chunk.writeOp(.neg_one);
                    return;
                }

                try self.compileExpression(unaryop.e.inner);
                if (unaryop.op == .negate) try self.chunk.writeOp(.negate);
            },
            .let => |let| {
                for (let.assignments) |a| {
                    try self.compileExpression(a.inner.expression.inner);
                    const l = try self.chunk.addLocal(a.inner.identifier);
                    try self.writeLocal(.set, l);
                }

                try self.compileExpression(let.in.inner);

                if (let.assignments.len == 1) {
                    self.chunk.popLocals(1);
                    try self.chunk.writeOp(.pop);
                    return;
                }

                var locals = @intCast(isize, let.assignments.len);
                self.chunk.popLocals(@intCast(u32, locals));
                while (locals > 0) : (locals -= std.math.maxInt(u8)) {
                    try self.chunk.writeOp(.pop_n);
                    try self.chunk.writeU8(@intCast(u8, std.math.min(std.math.maxInt(u8), locals)));
                }
            },
            .function => |f| {
                var comp = Compiler{
                    .gpa = self.gpa,
                    .chunk = bc.Chunk.init(self.gpa),
                    .parent = self,
                    .program = undefined,
                };

                var func = try comp.compileFunction("", &f);
                const c = try self.chunk.addConstant(func);
                try self.writeConst(c);
            },
            .apply => |a| {
                try self.compileExpression(a.f.inner);
                for (a.args) |arg| try self.compileExpression(arg.inner);
                try self.chunk.writeOp(.call);
                try self.chunk.writeU8(@intCast(u8, a.args.len));
            },
            .@"if" => |f| {
                try self.compileExpression(f.condition.inner);
                var to_else_jmp = try self.chunk.writeJmp(.jmpf8);

                try self.compileExpression(f.then.inner);
                var to_after_else_jmp = try self.chunk.writeJmp(.jmp8);

                try to_else_jmp.set();
                try self.compileExpression(f.@"else".inner);
                try to_after_else_jmp.set();
            },
        }
    }

    fn compileFunction(self: *Compiler, name: []const u8, f: *const parser.Function) error{ OutOfMemory, LocalNotFound }!bc.Value {
        for (f.params) |param| _ = try self.chunk.addLocal(param.inner.identifier);

        try self.compileExpression(f.body.inner);
        try self.chunk.writeOp(.ret);

        return bc.Value{ .function = .{ .chunk = self.chunk, .arity = @intCast(u32, f.params.len), .name = name } };
    }

    pub fn compile(self: *Compiler) !void {
        for (self.program.stmts.items) |stmt| {
            switch (stmt.inner) {
                .expression => |e| {
                    try self.compileExpression(e);
                    try self.chunk.writeOp(.pop);
                },
                .assignment => |a| {
                    const l = try self.chunk.addLocal(a.identifier);
                    try self.compileExpression(a.expression.inner);
                    try self.writeLocal(.set, l);
                },
            }
        }
    }

    pub fn deinit(self: *Compiler) void {
        self.chunk.deinit();
    }
};

const testing = std.testing;

fn testCompile(source: []const u8, disassembly: []const u8) !void {
    var l = parser.Lexer{ .real = lexer.Lexer.init(source) };

    var p = parser.Parser.init(testing.allocator, l);
    defer p.deinit();

    var program = try p.parse();
    defer program.deinit();

    try sema.normaliseProgram(&program);

    var c = Compiler.init(testing.allocator, &program);
    defer c.deinit();

    try c.compile();

    var al = std.ArrayList(u8).init(testing.allocator);
    defer al.deinit();

    try c.chunk.disassemble(al.writer());

    try std.testing.expectEqualStrings(disassembly, al.items);
}

test "constants" {
    try testCompile("1;-1;",
        \\ONE   
        \\POP   
        \\NONE  
        \\POP   
        \\
    );

    try testCompile("10;",
        \\CONST  c0    ; 10
        \\POP   
        \\
    );

    try testCompile("true;",
        \\TRUE  
        \\POP   
        \\
    );

    try testCompile("a = true;",
        \\TRUE  
        \\SET    l0   
        \\
    );
}

test "maths" {
    try testCompile("3*2 + 6/3;",
        \\CONST  c0    ; 3
        \\CONST  c1    ; 2
        \\MULT  
        \\CONST  c2    ; 6
        \\CONST  c3    ; 3
        \\DIV   
        \\ADD   
        \\POP   
        \\
    );
}

test "let..in" {
    try testCompile("let a = 1; in a + 2;",
        \\ONE   
        \\SET    l0   
        \\GET    l0   
        \\CONST  c0    ; 2
        \\ADD   
        \\POP   
        \\POP   
        \\
    );

    try testCompile("a = 5; b = let a = 1; in a + 2;",
        \\CONST  c0    ; 5
        \\SET    l0   
        \\ONE   
        \\SET    l2   
        \\GET    l2   
        \\CONST  c1    ; 2
        \\ADD   
        \\POP   
        \\SET    l1   
        \\
    );
}

test "function" {
    try testCompile("a = 1; f = fn x y => x + y; b = f 1 1;",
        \\============  ============
        \\GET    l0   
        \\GET    l1   
        \\ADD   
        \\RET   
        \\============================
        \\ONE   
        \\SET    l0   
        \\CONST  c0    ; <func >
        \\SET    l1   
        \\GET    l1   
        \\ONE   
        \\ONE   
        \\CALL    2   
        \\SET    l2   
        \\
    );
    try testCompile("f = fn x y => x + y; b = f 1 (f 2 3);",
        \\============  ============
        \\GET    l0   
        \\GET    l1   
        \\ADD   
        \\RET   
        \\============================
        \\CONST  c0    ; <func >
        \\SET    l0   
        \\GET    l0   
        \\ONE   
        \\GET    l0   
        \\CONST  c1    ; 2
        \\CONST  c2    ; 3
        \\CALL    2   
        \\CALL    2   
        \\SET    l1   
        \\
    );
}

test "if/then/elif/else" {
    try testCompile("if true then 1 else -1;",
        \\TRUE  
        \\JMPF   p6   
        \\ONE   
        \\JMP    p7   
        \\NONE  
        \\POP   
        \\
    );

    try testCompile("if 1 == 2 then 1 else -1;",
        \\ONE   
        \\CONST  c0    ; 2
        \\EQ    
        \\JMPF   p9   
        \\ONE   
        \\JMP    pa   
        \\NONE  
        \\POP   
        \\
    );
}
