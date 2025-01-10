#include <stdio.h>

#include "raylib.h"

void draw_text_ex_no_vec(Font* font, const char* text, float x, float y, float font_size, float spacing, Color* color) {
    DrawTextEx(*font, text, (Vector2){ x, y }, font_size, spacing, *color);
}
