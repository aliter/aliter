// Copyright (c) Athena Dev Teams
//             & Aliter Dev Teams

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <zlib.h>

#include "erl_nif.h"

struct map_cache_header {
    int file_size;
    short map_count;
};

struct map_cache_info {
    char name[12];
    short width;
    short height;
    int data_length;
};

struct map_data {
    int width;
    int height;
    unsigned char *cells;
};

struct tmp_path {
    short x, y, dist, before, cost, flag;
};

const char walk_choices [3][3] = {
    {1,0,7},
    {2,-1,6},
    {3,4,5}
};

struct map_data maps[1024];

#define MAX_WALKPATH 32
#define calc_index(x, y) (((x) + (y) * MAX_WALKPATH) & (MAX_WALKPATH * MAX_WALKPATH - 1))


void debug(const char *fmt, ...) {
    va_list ap;

    va_start(ap, fmt);

    FILE* fp = fopen("/Users/Alex/Projects/aliter/c.out", "a+");

    vfprintf(fp, fmt, ap);

    va_end(ap);

    fclose(fp);
}

int decompress(unsigned char *dest,
               int length,
               const unsigned char *source,
               unsigned long source_length) {
    z_stream stream;
    int result;

    stream.next_in = (Bytef *)source;
    stream.avail_in = (uInt)source_length;

    stream.next_out = (Bytef *)dest;
    stream.avail_out = (uInt)length;

    stream.zalloc = (alloc_func)0;
    stream.zfree = (free_func)0;

    result = inflateInit(&stream);
    if (result != Z_OK)
        return result;

    result = inflate(&stream, Z_FINISH);
    if (result != Z_STREAM_END) {
        inflateEnd(&stream);
        return (result == Z_OK) ? Z_BUF_ERROR : result ;
    }

    return inflateEnd(&stream);
}

void read_map(struct map_data *data, FILE *fp) {
    struct map_cache_info info;
    unsigned long length;

    unsigned char *zipped, *cells;

    debug("Reading info.\n");
    fread(&info, sizeof(struct map_cache_info), 1, fp);

    debug("Reading zipped cells.\n");

    zipped = (unsigned char *)malloc(info.data_length);
    fread(zipped, info.data_length, 1, fp);

    debug("Decompressing cells.\n");

    data->width = info.width;
    data->height = info.height;

    length = info.width * info.height;

    cells = (unsigned char *)malloc(length);
    decompress(cells, length, (const Bytef *)zipped, (uLong)info.data_length);

    data->cells = cells;

    debug("Decompressed!\n");
}

void read_all_maps(const char *file) {
    struct map_cache_header header;

    FILE *fp;
    int i;

    if ((fp = fopen(file, "rb")) == NULL) {
        debug("Could not open map cache file.\n");
        exit(EXIT_FAILURE);
    }

    fseek(fp, 0, SEEK_SET);
    fread(&header, sizeof(struct map_cache_header), 1, fp);

    for (i = 0; i < header.map_count; i++) {
        debug("Reading map %d\n", i);
        read_map(&maps[i], fp);
    }
}

unsigned char at(struct map_data map,
                 int x,
                 int y) {
    int offset = map.width * y + x;

    if (offset + 1 > (map.width * map.height))
        return 1;
    else
        return map.cells[offset];
}

void printmap(struct map_data map) {
    int i, j;

    for (i = 0; i < map.height; i++) {
        for (j = 0; j < map.width; j++)
            debug("%d", at(map, j, i));

        debug("\n");
    }
}

static void push_heap_path(int *heap,struct tmp_path *tp,int index) {
    int i,h;

    h = heap[0];
    heap[0]++;

    for (i = (h - 1) / 2; h > 0 && tp[index].cost < tp[heap[i + 1]].cost; i = (h - 1) / 2)
        heap[h + 1] = heap[i + 1], h = i;

    heap[h + 1] = index;
}

static void update_heap_path(int *heap,struct tmp_path *tp,int index) {
    int i, h;

    for(h = 0; h < heap[0]; ++h)
        if(heap[h + 1] == index)
            break;

    if (h == heap[0]) {
        debug("update_heap_path bug\n");
        exit(EXIT_FAILURE);
    }

    for (i = (h - 1) / 2; h > 0 && tp[index].cost < tp[heap[i + 1]].cost; i = (h - 1) / 2)
        heap[h + 1] = heap[i + 1], h = i;

    heap[h + 1] = index;
}

static int pop_heap_path(int *heap,struct tmp_path *tp) {
    int i,h,k;
    int ret,last;

    if (heap[0] <= 0)
        return -1;

    ret = heap[1];
    last = heap[heap[0]];
    heap[0]--;

    for (h = 0, k = 2; k < heap[0]; k = k * 2 + 2 ) {
        if(tp[heap[k+1]].cost > tp[heap[k]].cost)
            k--;

        heap[h + 1] = heap[k + 1], h = k;
    }

    if (k == heap[0])
        heap[h + 1] = heap[k], h = k-1;

    for (i = (h - 1) / 2; h > 0 && tp[heap[i + 1]].cost > tp[last].cost; i = (h - 1) / 2 )
        heap[h+1] = heap[i+1], h = i;

    heap[h + 1] = last;

    return ret;
}

static int calc_cost(struct tmp_path *p, int x1, int y1) {
    int xd = abs(x1 - p->x);
    int yd = abs(y1 - p->y);
    return (xd + yd) * 10 + p->dist;
}

static int add_path(int *heap,struct tmp_path *tp,int x,int y,int dist,int before,int cost) {
    int i;

    i = calc_index(x,y);

    if (tp[i].x == x && tp[i].y == y) {
        if(tp[i].dist > dist) {
            tp[i].dist = dist;
            tp[i].before = before;
            tp[i].cost = cost;

            if(tp[i].flag)
                push_heap_path(heap, tp, i);
            else
                update_heap_path(heap, tp, i);

            tp[i].flag = 0;
        }
        return 0;
    }

    if(tp[i].x || tp[i].y)
        return 1;

    tp[i].x = x;
    tp[i].y = y;
    tp[i].dist = dist;
    tp[i].before = before;
    tp[i].cost = cost;
    tp[i].flag = 0;
    push_heap_path(heap, tp, i);

    return 0;
}

ERL_NIF_TERM finish(ErlNifEnv *env, ERL_NIF_TERM *path, ERL_NIF_TERM *step, int length) {
    int i;
    ERL_NIF_TERM list = enif_make_list(env, 0);

    for (i = length - 1; i >= 0; i--) {
        list = enif_make_list_cell(env, path[i], list);
    }

    free(step);
    free(path);

    return list;
}

ERL_NIF_TERM pathfind(ErlNifEnv *env,
                      int argc,
                      const ERL_NIF_TERM argv[]) {
    /*debug("----------------------------\n");*/

    if (argc < 3)
      return enif_make_list(env, 0);

    ERL_NIF_TERM eid = argv[0];
    ERL_NIF_TERM from = argv[1];
    ERL_NIF_TERM to = argv[2];

    int id, x0, y0, x1, y1;

    // Get the map ID
    enif_get_int(env, eid, &id);

    ERL_NIF_TERM head, tail;

    // Get the From X, Y
    enif_get_list_cell(env, from, &head, &tail);
    enif_get_int(env, head, &x0);
    enif_get_int(env, tail, &y0);

    // Get the To X, Y
    enif_get_list_cell(env, to, &head, &tail);
    enif_get_int(env, head, &x1);
    enif_get_int(env, tail, &y1);

    struct map_data map = maps[id];
    struct tmp_path tp[MAX_WALKPATH * MAX_WALKPATH];

    int heap[151];
    int rp, xs, ys;

    ERL_NIF_TERM *path;
    ERL_NIF_TERM *step;

    register int
        i = 0,
        j = 0,
        len,
        x = x0,
        y = y0,
        dx = ((dx = x1 - x0) ? ((dx < 0) ? -1 : 1) : 0),
        dy = ((dy = y1 - y0) ? ((dy < 0) ? -1 : 1) : 0);

    /* printmap(map); */

    /* debug("Pathfinding.\n"); */
    /* debug("\tMap: %d (%d x %d)\n", id, map.width, map.height); */
    /* debug("\tFrom: (%d, %d)\n", x0, y0); */
    /* debug("\tTo: (%d, %d)\n", x1, y1); */
    /* debug("\tFirst step: %d\n", at(map, x + dx, y + dy)); */

    step = (ERL_NIF_TERM *)malloc(3 * sizeof(ERL_NIF_TERM));
    path = (ERL_NIF_TERM *)malloc(MAX_WALKPATH * sizeof(ERL_NIF_TERM));

    for (i = 0;
         i < MAX_WALKPATH &&
             (x != x1 || y != y1) &&
             at(map, x + dx, y + dy) == 0;
         i++) {
        x += dx;
        y += dy;

        /* debug("OK: (%d, %d)\n", x, y); */

        step[0] = enif_make_int(env, x);
        step[1] = enif_make_int(env, y);
        step[2] = enif_make_int(env, walk_choices[-dy + 1][dx + 1]);

        path[i] = enif_make_tuple(env, 3, step[0], step[1], step[2]);

        if (x == x1)
            dx = 0;
        if (y == y1)
            dy = 0;

        /* debug("Next cell? %d (Done: %d)\n", at(map, x + dx, y + dy), dx == 0 && dy == 0); */
    }

    // Simple pathfinding was successful
    if (x == x1 && y == y1)
        return finish(env, path, step, i);

    memset(tp, 0, sizeof(tp));

    i = calc_index(x0,y0);
    tp[i].x = x0;
    tp[i].y = y0;
    tp[i].dist = 0;
    tp[i].before = 0;
    tp[i].cost = calc_cost(&tp[i], x1, y1);
    tp[i].flag = 0;
    heap[0] = 0;
    push_heap_path(heap, tp, calc_index(x0,y0));
    xs = map.width - 1; // あらかじめ１減算しておく
    ys = map.height - 1;


    while (1) {
        int e = 0,
            f = 0,
            dist,
            cost,
            dc[4] = {0, 0, 0, 0};

        if(heap[0] == 0)
            return finish(env, path, step, 0);

        rp = pop_heap_path(heap,tp);
        x = tp[rp].x;
        y = tp[rp].y;
        dist = tp[rp].dist + 10;
        cost = tp[rp].cost;

        if (x == x1 && y == y1)
            break;

        // dc[0] : y++ の時のコスト増分
        // dc[1] : x-- の時のコスト増分
        // dc[2] : y-- の時のコスト増分
        // dc[3] : x++ の時のコスト増分

        if (y < ys && !at(map, x, y + 1)) {
            f |= 1;
            dc[0] = (y >= y1 ? 20 : 0);
            e += add_path(heap, tp, x, y + 1, dist, rp, cost + dc[0]); // (x,   y+1)
        }
        if (x > 0 && !at(map, x - 1, y)) {
            f |= 2;
            dc[1] = (x <= x1 ? 20 : 0);
            e += add_path(heap, tp, x - 1, y, dist, rp, cost + dc[1]); // (x-1, y  )
        }
        if (y > 0 && !at(map, x, y - 1)) {
            f |= 4;
            dc[2] = (y <= y1 ? 20 : 0);
            e += add_path(heap, tp, x, y - 1, dist, rp, cost + dc[2]); // (x  , y-1)
        }
        if (x < xs && !at(map, x + 1, y)) {
            f |= 8;
            dc[3] = (x >= x1 ? 20 : 0);
            e += add_path(heap, tp, x + 1, y, dist, rp, cost + dc[3]); // (x+1, y  )
        }

        if((f & (2+1)) == (2+1) && !at(map, x - 1, y + 1))
            e += add_path(heap, tp, x - 1, y + 1, dist + 4, rp, cost + dc[1] + dc[0] - 6); // (x-1, y+1)

        if((f & (2+4)) == (2+4) && !at(map, x - 1, y - 1))
            e += add_path(heap, tp, x - 1, y - 1, dist + 4, rp, cost + dc[1] + dc[2] - 6); // (x-1, y-1)

        if((f & (8+4)) == (8+4) && !at(map, x + 1, y - 1))
            e += add_path(heap, tp, x + 1, y - 1, dist + 4, rp, cost + dc[3] + dc[2] - 6); // (x+1, y-1)

        if((f & (8+1)) == (8+1) && !at(map, x + 1, y + 1))
            e += add_path(heap, tp, x + 1, y + 1, dist + 4, rp, cost + dc[3] + dc[0] - 6); // (x+1, y+1)

        tp[rp].flag = 1;

        if (e || heap[0] >= 150 - 5)
            return finish(env, path, step, 0);
    }

    for (len = 0, i = rp; len < 100 && i != calc_index(x0, y0); i = tp[i].before, len++);

    if (len == 100 || len >= MAX_WALKPATH)
        return finish(env, path, step, 0);

    for (i = rp, j = len - 1; j >= 0; i = tp[i].before, j--) {
        int dx = tp[i].x - tp[tp[i].before].x;
        int dy = tp[i].y - tp[tp[i].before].y;
        /* int dir; */

        step[0] = enif_make_int(env, tp[i].x);
        step[1] = enif_make_int(env, tp[i].y);

        /* if (dx == 0) */
        /*     dir = (dy > 0 ? 0 : 4); */
        /* else if (dx > 0) */
        /*     dir = (dy == 0 ? 6 : (dy < 0 ? 5 : 7)); */
        /* else */
        /*     dir = (dy == 0 ? 2 : (dy > 0 ? 1 : 3)); */

        step[2] = enif_make_int(env, walk_choices[-dy + 1][dx + 1]);

        path[j] = enif_make_tuple(env, 3, step[0], step[1], step[2]);
    }

    return finish(env, path, step, len);
}

static ErlNifFunc funcs[] = {
    {"pathfind", 3, pathfind}
};

int load(ErlNifEnv *env,
         void **priv_data,
         ERL_NIF_TERM load_info) {
    debug("erl_nif loading.\n");
    read_all_maps("/Users/Alex/Projects/aliter/priv/maps");
    return 0;
}

void unload(ErlNifEnv *env,
            void *priv_data) {
    debug("erl_nif unloading.\n");
}

int upgrade(ErlNifEnv *env,
            void **priv_data,
            void **old_priv_data,
            ERL_NIF_TERM load_info) {
    debug("erl_nif upgrading.\n");
    return 0;
}

ERL_NIF_INIT(nif, funcs, load, load, upgrade, unload);
