const std = @import("std");

pub const Fake = struct {
    toks: []const Tok,
    index: u32 = 0,

    pub fn next(self: *Fake) ?TokLoc {
        if (self.index > self.toks.len - 1) return null;

        const tok = self.toks[self.index];
        self.index += 1;

        return .{ .tok = tok, .loc = .{} };
    }
};

/// Tok lists all the tokens that can be lexed.
pub const Tok = union(enum) {
    identifier: []const u8,
    integer: i64,
    true,
    false,

    equals,
    plus,
    minus,
    forward_slash,
    asterisk,
    eq,
    neq,
    lt,
    gt,
    lte,
    gte,

    colon,
    semicolon,
    left_paren,
    right_paren,
    left_curly,
    right_curly,

    let,
    in,
    @"fn",
    fat_arrow,
    @"if",
    elif,
    @"else",
    then,

    @"union",
};

/// Loc represents a locaiton in the source code. It is in a human-readable format - i.e. line and col start at 1.
pub const Loc = struct { line: u32 = 1, col: u32 = 1 };

/// TokLoc represents a token and its location in the source code.
pub const TokLoc = struct { tok: Tok, loc: Loc };

/// Diag will be filled in with diagnostic information if an error happens whilst lexing.
pub const Diag = struct {
    loc: Loc,
    msg: []const u8,
    allocator: ?std.mem.Allocator = null,

    pub fn deinit(self: *Diag) void {
        var a = self.allocator orelse return;
        a.free(self.msg);
    }
};

pub const Lexer = struct {
    source: []const u8,
    loc: Loc = .{},
    index: u32 = 0,

    peeked: ?u8 = null,
    diag: ?Diag = null,

    pub const Error = error{ EndOfFile, UnexpectedChar };

    pub fn init(source: []const u8) Lexer {
        return .{ .source = source };
    }

    fn peek(self: *Lexer) error{EndOfFile}!u8 {
        if (self.peeked) |c| return c;

        if (self.index >= self.source.len) return error.EndOfFile;

        return self.source[self.index];
    }

    fn pop(self: *Lexer) error{EndOfFile}!u8 {
        if (self.peeked) |c| {
            self.peeked = null;
            return c;
        }

        if (self.index >= self.source.len) return error.EndOfFile;

        const c = self.source[self.index];
        self.index += 1;

        if (c == '\n') {
            self.loc.line += 1;
            self.loc.col = 0;
        } else {
            self.loc.col += 1;
        }

        return c;
    }

    fn skipWhitespace(self: *Lexer) void {
        while (true) {
            const c = self.peek() catch return;

            switch (c) {
                '\r', '\n', '\t', ' ' => _ = self.pop() catch unreachable,
                else => return,
            }
        }
    }

    fn tokloc(self: *Lexer, t: Tok) TokLoc {
        const loc = self.loc;
        _ = self.pop() catch unreachable;
        return .{ .tok = t, .loc = loc };
    }

    fn parseIdentifier(self: *Lexer) !TokLoc {
        const start_loc = self.loc;
        const start = self.index;
        var end = self.index + 1;
        _ = self.pop() catch unreachable;

        while (true) {
            const c = self.peek() catch return TokLoc{
                .tok = .{ .identifier = self.source[start..end] },
                .loc = start_loc,
            };

            if ((c >= 'A' and c <= 'z') or (c >= '0' and c <= '9') or c == '-') {
                end += 1;
                _ = self.pop() catch unreachable;
                continue;
            }

            return TokLoc{ .tok = .{ .identifier = self.source[start..end] }, .loc = start_loc };
        }
    }

    fn parseNumber(self: *Lexer) Error!TokLoc {
        const start_loc = self.loc;
        const start = self.index;
        var end = self.index + 1;

        _ = self.pop() catch unreachable;

        const src = b: {
            while (true) {
                const c = self.peek() catch break :b self.source[start..end];
                if ((c >= '0' and c <= '9') or (c >= 'a' and c <= 'f') or c == 'x' or c == 'o' or c == 'b' or c == '_') {
                    end += 1;
                    _ = self.pop() catch unreachable;
                    continue;
                }

                break :b self.source[start..end];
            }
        };

        const num = std.fmt.parseInt(i64, src, 0) catch {
            self.diag = .{ .msg = "invaid character in number", .loc = start_loc };
            return error.UnexpectedChar;
        };

        return TokLoc{ .tok = .{ .integer = num }, .loc = start_loc };
    }

    /// parseKeyword parses the given keyword.
    fn parseKeyword(self: *Lexer, expected: []const u8, tok: Tok) !TokLoc {
        const loc = self.loc;
        const left = self.source.len - self.index;
        if (left < expected.len) return try self.parseIdentifier();

        if (!std.mem.eql(u8, expected, self.source[self.index .. self.index + expected.len]))
            return try self.parseIdentifier();

        if (left >= expected.len + 1) {
            const c = self.source[self.index + expected.len];
            if ((c >= 'A' and c <= 'Z') or (c >= 'a' and c <= 'z') or (c >= '0' and c <= '9') or c == '-')
                return try self.parseIdentifier();
        }

        self.peeked = null;
        self.index += @intCast(u32, expected.len);
        self.loc.col += @intCast(u32, expected.len);
        return TokLoc{ .loc = loc, .tok = tok };
    }

    /// next returns the next token or null if we have reached the end of the file.
    pub fn next(self: *Lexer) Error!?TokLoc {
        self.skipWhitespace();

        const c = self.peek() catch return null;

        return switch (c) {
            '=' => {
                const loc = self.loc;
                _ = self.pop() catch unreachable;
                const c2 = self.peek() catch return .{ .tok = .equals, .loc = loc };
                return switch (c2) {
                    '>' => b: {
                        _ = self.pop() catch unreachable;
                        break :b .{ .tok = .fat_arrow, .loc = loc };
                    },
                    '=' => b: {
                        _ = self.pop() catch unreachable;
                        break :b .{ .tok = .eq, .loc = loc };
                    },
                    else => .{ .tok = .equals, .loc = loc },
                };
            },
            '<' => {
                const loc = self.loc;
                _ = self.pop() catch unreachable;
                const c2 = self.peek() catch return .{ .tok = .lt, .loc = loc };
                return switch (c2) {
                    '=' => b: {
                        _ = self.pop() catch unreachable;
                        break :b .{ .tok = .lte, .loc = loc };
                    },
                    else => .{ .tok = .lt, .loc = loc },
                };
            },
            '>' => {
                const loc = self.loc;
                _ = self.pop() catch unreachable;
                const c2 = self.peek() catch return .{ .tok = .gt, .loc = loc };
                return switch (c2) {
                    '=' => b: {
                        _ = self.pop() catch unreachable;
                        break :b .{ .tok = .gte, .loc = loc };
                    },
                    else => .{ .tok = .gt, .loc = loc },
                };
            },
            '!' => {
                const loc = self.loc;
                _ = self.pop() catch unreachable;
                const c2 = self.pop() catch return error.UnexpectedChar;
                if (c2 != '=') return error.UnexpectedChar;
                return .{ .loc = loc, .tok = .neq };
            },
            '+' => self.tokloc(.plus),
            '-' => self.tokloc(.minus),
            '/' => self.tokloc(.forward_slash),
            '*' => self.tokloc(.asterisk),
            ':' => self.tokloc(.colon),
            ';' => self.tokloc(.semicolon),
            '(' => self.tokloc(.left_paren),
            ')' => self.tokloc(.right_paren),
            '{' => self.tokloc(.left_curly),
            '}' => self.tokloc(.right_curly),
            'e' => {
                if (self.index + 2 >= self.source.len or self.source[self.index + 1] != 'l')
                    return try self.parseIdentifier();

                return switch (self.source[self.index + 2]) {
                    'i' => try self.parseKeyword("elif", .elif),
                    's' => try self.parseKeyword("else", .@"else"),

                    else => try self.parseIdentifier(),
                };
            },
            'f' => {
                if (self.index + 1 >= self.source.len) return try self.parseIdentifier();

                return switch (self.source[self.index + 1]) {
                    'a' => try self.parseKeyword("false", .false),
                    'n' => try self.parseKeyword("fn", .@"fn"),
                    else => try self.parseIdentifier(),
                };
            },
            'i' => {
                if (self.index + 1 >= self.source.len) return try self.parseIdentifier();

                return switch (self.source[self.index + 1]) {
                    'f' => try self.parseKeyword("if", .@"if"),
                    'n' => try self.parseKeyword("in", .in),
                    else => try self.parseIdentifier(),
                };
            },
            'l' => try self.parseKeyword("let", .let),
            't' => {
                if (self.index + 1 >= self.source.len) return try self.parseIdentifier();

                return switch (self.source[self.index + 1]) {
                    'h' => try self.parseKeyword("then", .then),
                    'r' => try self.parseKeyword("true", .true),
                    else => try self.parseIdentifier(),
                };
            },
            'u' => try self.parseKeyword("union", .@"union"),
            else => {
                if ((c >= 'A' and c <= 'Z') or (c >= 'a' and c <= 'z')) return try self.parseIdentifier();

                if (c >= '0' and c <= '9') return try self.parseNumber();

                self.diag = .{ .msg = "unexpected character", .loc = self.loc };
                return error.UnexpectedChar;
            },
        };
    }
};

const testing = std.testing;

const TestError = Lexer.Error || error{ OutOfMemory, TestExpectedEqual };

/// sourceToTokLocs lexes source into a slice of TokLocs.
fn sourceToTokLocs(gpa: std.mem.Allocator, source: []const u8) TestError![]TokLoc {
    var arr = std.ArrayListUnmanaged(TokLoc){};
    errdefer arr.deinit(gpa);

    var lexer = Lexer.init(source);
    while (try lexer.next()) |tokloc| {
        try arr.append(gpa, tokloc);
    }

    return arr.toOwnedSlice(gpa);
}

/// printTokSlice prints out a list of toks separated by commas.
fn printTokSlice(toks: []const Tok) void {
    std.debug.print("[", .{});

    for (toks) |t, i| {
        std.debug.print("{}", .{t});
        if (i < toks.len - 1) std.debug.print(", ", .{});
    }

    std.debug.print("]\n", .{});
}

/// printTokSlices prints out the expected tokens and the actual tokens.
fn printTokSlices(expected: []const Tok, actual: []const Tok) void {
    std.debug.print("=========== expected ===========\n", .{});
    printTokSlice(expected);
    std.debug.print("============ actual ============\n", .{});
    printTokSlice(actual);
}

/// expectLex lexes source and compares it to expected.
fn expectLex(gpa: std.mem.Allocator, source: []const u8, expected: []const Tok) TestError!void {
    const toklocs = try sourceToTokLocs(gpa, source);
    defer gpa.free(toklocs);

    var actual = std.ArrayListUnmanaged(Tok){};
    defer actual.deinit(gpa);
    for (toklocs) |tokloc| {
        try actual.append(gpa, tokloc.tok);
    }

    if (actual.items.len != expected.len) {
        std.debug.print("slices wrong size: expected length {}, not {}\n", .{ expected.len, actual.items.len });
        printTokSlices(expected, actual.items);
        return error.TestExpectedEqual;
    }

    for (actual.items) |item_actual, i| {
        const item_expected = expected[i];
        if (std.meta.activeTag(item_actual) != std.meta.activeTag(item_expected)) {
            std.debug.print("at index {}: expected {}, got {}\n", .{ i, item_expected, item_actual });
            printTokSlices(expected, actual.items);
            return error.TestExpectedEqual;
        }

        switch (item_actual) {
            .identifier => |s| if (std.mem.eql(u8, s, item_expected.identifier)) continue,
            .integer => |int| if (int == item_expected.integer) continue,
            .equals, .plus, .minus, .forward_slash, .asterisk => continue,
            .left_paren, .right_paren, .left_curly, .right_curly => continue,
            .semicolon, .colon => continue,
            .let, .in, .@"fn", .fat_arrow, .@"if", .@"else", .elif, .then => continue,
            .true, .false, .@"union" => continue,
            .eq, .neq, .gt, .gte, .lt, .lte => continue,
        }

        std.debug.print("at index {}: expected {}, got {}\n", .{ i, item_expected, item_actual });
        printTokSlices(expected, actual.items);
        return error.TestExpectedEqual;
    }
}

test "identifiers" {
    try expectLex(testing.allocator, "f foo Bar BAZ", &.{
        .{ .identifier = "f" },
        .{ .identifier = "foo" },
        .{ .identifier = "Bar" },
        .{ .identifier = "BAZ" },
    });
}

test "fail: identifiers" {
    try testing.expectError(error.UnexpectedChar, sourceToTokLocs(testing.allocator, "_foo"));
}

test "integers" {
    try expectLex(testing.allocator, "147 0x147ef", &.{
        .{ .integer = 147 },
        .{ .integer = 0x147ef },
    });
}

test "maths" {
    try expectLex(testing.allocator, "x = 5 * (3 - 2)", &.{
        .{ .identifier = "x" },
        .equals,
        .{ .integer = 5 },
        .asterisk,
        .left_paren,
        .{ .integer = 3 },
        .minus,
        .{ .integer = 2 },
        .right_paren,
    });
}

test "fail: random chars" {
    try testing.expectError(error.UnexpectedChar, sourceToTokLocs(testing.allocator, "~"));
    try testing.expectError(error.UnexpectedChar, sourceToTokLocs(testing.allocator, "^"));
    try testing.expectError(error.UnexpectedChar, sourceToTokLocs(testing.allocator, "#"));
}

test "keywords" {
    try expectLex(testing.allocator, "true", &.{.true});
    try expectLex(testing.allocator, "false", &.{.false});
    try expectLex(testing.allocator, "fn", &.{.@"fn"});
    try expectLex(testing.allocator, "truf", &.{.{ .identifier = "truf" }});
    try expectLex(testing.allocator, "truef", &.{.{ .identifier = "truef" }});
    try expectLex(testing.allocator, "true f", &.{ .true, .{ .identifier = "f" } });
    try expectLex(testing.allocator, "true+f", &.{ .true, .plus, .{ .identifier = "f" } });
    try expectLex(testing.allocator, "true-f", &.{.{ .identifier = "true-f" }});
    try expectLex(testing.allocator, "let", &.{.let});
    try expectLex(testing.allocator, "in", &.{.in});
    try expectLex(testing.allocator, "if then elif else", &.{ .@"if", .then, .elif, .@"else" });
}

test "boolean logic" {
    try expectLex(testing.allocator, "= == != > < >= <=", &.{ .equals, .eq, .neq, .gt, .lt, .gte, .lte });
}

test "signatures" {
    try expectLex(testing.allocator, "foo : fn Int a => b", &.{
        .{ .identifier = "foo" },
        .colon,
        .@"fn",
        .{ .identifier = "Int" },
        .{ .identifier = "a" },
        .fat_arrow,
        .{ .identifier = "b" },
    });
}
