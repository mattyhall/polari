const std = @import("std");

pub fn main() !void {}

test "all" {
    const lexer = @import("lexer.zig");
    _ = lexer;
    const parser = @import("parser.zig");
    _ = parser;
    std.testing.refAllDecls(@This());
}
