const std = @import("std");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const utils = @import("utils.zig");
const simargs = @import("simargs");

pub const log_level: std.log.Level = .info;

pub const Opts = struct {};

fn readStdin(gpa: std.mem.Allocator) ![]const u8 {
    var stdin = std.io.getStdIn();
    return try stdin.reader().readAllAlloc(gpa, 10 * 1024 * 1024);
}

fn readFile(gpa: std.mem.Allocator, path: []const u8) ![]const u8 {
    var abs_path = if (std.fs.path.isAbsolute(path))
        try gpa.dupe(u8, path)
    else
        try std.fs.cwd().realpathAlloc(gpa, path);

    defer gpa.free(abs_path);

    var f = try std.fs.openFileAbsolute(abs_path, .{});
    defer f.close();

    return try f.reader().readAllAlloc(gpa, 10 * 1024 * 1024);
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){ .backing_allocator = std.heap.page_allocator };
    defer _ = gpa.deinit();

    var opts = try simargs.parse(gpa.allocator(), Opts);
    defer opts.deinit();

    var source = if (opts.positional_args.items.len == 0)
        try readStdin(gpa.allocator())
    else
        try readFile(gpa.allocator(), opts.positional_args.items[0]);

    defer gpa.allocator().free(source);

    var l = lexer.Lexer{ .source = source };
    var p = parser.Parser.init(gpa.allocator(), .{ .real = l });
    defer p.deinit();

    var program = p.parse() catch |e| {
        switch (e) {
            error.EndOfFile => std.debug.print("end of file", .{}),
            else => {
                var diag = p.getDiag() orelse std.os.exit(1);
                try utils.printError(source, diag);
            },
        }

        std.os.exit(1);
    };
    program.deinit();
}

const Self = @This();

test "all" {
    std.testing.refAllDeclsRecursive(Self);
}
