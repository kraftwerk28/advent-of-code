#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define FILE_COUNT_CAP 256

struct entry {
	unsigned long size;
	char name[256];
	struct entry *parent;
	unsigned file_count;
	struct entry *files[FILE_COUNT_CAP];
};

static struct entry *new_entry() {
	return (struct entry *)calloc(1, sizeof(struct entry));
}

static void add_file(struct entry *dir, struct entry *file) {
	dir->files[dir->file_count] = file;
	dir->file_count++;
}

static void entry_free(struct entry *entry) {
	if (entry->size == 0) {
		for (int i = 0; i < entry->file_count; i++) {
			entry_free(entry->files[i]);
		}
	}
	free(entry);
}

static unsigned long entry_total_size(const struct entry *e) {
	unsigned long result = e->size;
	for (int i = 0; i < e->file_count; i++) {
		struct entry *child = e->files[i];
		result += entry_total_size(child);
	}
	return result;
}

static struct entry *cd(const struct entry *dir, const char *name) {
	for (int i = 0; i < dir->file_count; i++) {
		struct entry *child = dir->files[i];
		if (strcmp(name, child->name) == 0) {
			return child;
		}
	}
	return NULL;
}

static void entry_print(const struct entry *e, FILE *stream, const int indent) {
	char *indentstr = (char *)calloc(indent * 2 + 1, sizeof(char));
	memset(indentstr, ' ', indent * 2);
	if (e->file_count > 0) {
		fprintf(stream, "%s- %s (dir)\n", indentstr, e->name);
		for (int i = 0; i < e->file_count; i++) {
			entry_print(e->files[i], stream, indent + 1);
		}
	} else {
		fprintf(
			stream, "%s- %s (file, size=%zu)\n", indentstr, e->name, e->size);
	}
	free(indentstr);
}
