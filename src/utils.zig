const std = @import("std");
const lexer = @import("lexer.zig");

const LocIndexes = struct {
    loc: u32,
    line_start: u32,
    line_end: u32,

    fn line(self: LocIndexes, source: []const u8) []const u8 {
        return source[self.line_start..self.line_end];
    }

    fn locLineIndex(self: LocIndexes) u32 {
        return self.loc - self.line_start;
    }
};

fn locToIndexes(source: []const u8, loc: lexer.Loc) error{EndOfFile}!LocIndexes {
    var line: u32 = 1;
    var col: u32 = 1;
    var index: u32 = 0;
    var line_start: u32 = 0;

    while (index < source.len) : (index += 1) {
        if (line == loc.line and col == loc.col) break;

        const c = source[index];
        if (c == '\n') {
            line += 1;
            col = 1;
            line_start = index + 1;
            continue;
        }

        col += 1;
    }

    if (index == source.len - 1) return error.EndOfFile;

    const loc_index = index;

    while (index < source.len and source[index] != '\n') : (index += 1) {}

    return LocIndexes{ .loc = loc_index, .line_start = line_start, .line_end = index };
}

pub fn printErrors(source: []const u8, diags: []const lexer.Diag) !void {
    for (diags) |diag| {
        try printError(source, diag);
    }
}

fn printError(source: []const u8, diag: lexer.Diag) !void {
    const indexes = try locToIndexes(source, diag.loc);

    std.debug.print("error {}:{}\n", .{ diag.loc.line, diag.loc.col });

    var num_buf: [128]u8 = undefined;
    const num = try std.fmt.bufPrint(&num_buf, "{}| ", .{diag.loc.line});

    std.debug.print("{s}{s}\n", .{ num, indexes.line(source) });

    {
        var i: u32 = 0;
        while (i < indexes.loc + num.len) : (i += 1) {
            std.debug.print(" ", .{});
        }

        std.debug.print("^\n", .{});
    }

    std.debug.print("{s}\n", .{diag.msg});
}
