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

    fn writeLocal(self: *Compiler, i: enum { get, set }, v: u32) !void {
        if (v > std.math.maxInt(u16)) {
            try self.chunk.writeOp(.rare);
            try self.chunk.writeRateOp(if (i == .get) .get32 else .set32);
            try self.chunk.writeU32(@intCast(u32, v));
        } else if (v > std.math.maxInt(u8)) {
            try self.chunk.writeOp(.rare);
            try self.chunk.writeRateOp(if (i == .get) .get16 else .set16);
            try self.chunk.writeU16(@intCast(u16, v));
        } else {
            try self.chunk.writeOp(if (i == .get) .get8 else .set8);
            try self.chunk.writeU8(@intCast(u8, v));
        }
    }

    fn compileExpression(self: *Compiler, expr: *const parser.Expression) !void {
        switch (expr.*) {
            .integer => |i| if (i == 1 or i == -1)
                try self.chunk.writeOp(if (i == 1) .one else .neg_one)
            else
                try self.writeConst(try self.chunk.addConstant(.{ .integer = i })),
            .boolean => |b| try self.chunk.writeOp(if (b) .true else .false),
            .identifier => |n| try self.writeLocal(.get, self.chunk.getLocal(n) orelse unreachable),
            .binop => |binop| {
                try self.compileExpression(binop.lhs.inner);
                try self.compileExpression(binop.rhs.inner);
                try self.chunk.writeOp(switch (binop.op) {
                    .plus => .add,
                    .minus => .subtract,
                    .multiply => .multiply,
                    .divide => .divide,
                });
            },
            .unaryop => |unaryop| {
                try self.compileExpression(unaryop.e.inner);
                try self.chunk.writeOp(.negate);
            },
            .let => @panic("not implemented"),
        }
    }

    pub fn compile(self: *Compiler) !void {
        for (self.program.stmts.items) |stmt| {
            switch (stmt.inner) {
                .expression => |e| try self.compileExpression(e),
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

    var c = Compiler.init(testing.allocator, &program);
    defer c.deinit();

    try c.compile();

    var al = std.ArrayList(u8).init(testing.allocator);
    defer al.deinit();

    try c.chunk.diassemble(al.writer());

    try std.testing.expectEqualStrings(disassembly, al.items);
}

test "constants" {
    try testCompile("10;",
        \\CONST  c0    ; 10
        \\
    );

    try testCompile("true;",
        \\TRUE  
        \\
    );

    try testCompile("a = true;",
        \\TRUE  
        \\SET    l0    ; a
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
        \\
    );
}
