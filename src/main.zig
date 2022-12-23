const std = @import("std");

pub fn main() !void {}

test "all" {
    const lexer = @import("lexer.zig");
    _ = lexer;
    std.testing.refAllDecls(@This());
}
