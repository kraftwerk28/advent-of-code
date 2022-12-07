#include <assert.h>
#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "entry.h"

#define PART_1_THRESHOLD 100000
#define PART_2_TOTAL_SPACE 70000000
#define PART_2_REQUIRED_SPACE 30000000

struct entry *parse(FILE *stream) {
	char *line = NULL;
	size_t alloc_len;
	int line_len;
	bool listing = false;
	struct entry *current = NULL;
	struct entry *root = new_entry();
	strcpy(root->name, "/");
	while ((line_len = getline(&line, &alloc_len, stdin)) > 0) {
		if (line[line_len - 1] == '\n')
			line[line_len - 1] = '\0';
		if (line[line_len - 2] == '\r')
			line[line_len - 2] = '\0';
		if (strncmp(line, "$ cd /", 6) == 0) {
			listing = false;
			current = root;
		} else if (strncmp(line, "$ cd ..", 7) == 0) {
			listing = false;
			current = current->parent;
		} else if (strncmp(line, "$ cd", 4) == 0) {
			listing = false;
			const char *target_dir = &line[5];
			current = cd(current, target_dir);
		} else if (strncmp(line, "$ ls", 4) == 0) {
			listing = true;
		} else if (listing) {
			char *delim = strchr(line, ' ');
			struct entry *child = new_entry();
			strncpy(child->name, &delim[1], 255);
			child->parent = current;
			if (strncmp(line, "dir", 3) == 0) {
				// This is a directory
				child->size = 0;
			} else {
				child->size = strtol(line, NULL, 10);
			}
			add_file(current, child);
		}
	}
	if (line) {
		free(line);
	}
	return root;
}

unsigned long part_1_aux(const struct entry *e) {
	unsigned long result = 0;
	if (e->file_count > 0) {
		unsigned long s = entry_total_size(e);
		if (s <= PART_1_THRESHOLD)
			result += s;
		for (int i = 0; i < e->file_count; i++)
			result += part_1_aux(e->files[i]);
	}
	return result;
}

void part_1(const struct entry *root) {
	unsigned long result = 0;
	for (int i = 0; i < root->file_count; i++) {
		result += part_1_aux(root->files[i]);
	}
	printf("Part 1: %zu\n", result);
}

void part_2_aux(
	const struct entry *e, unsigned long threshold, unsigned long *min) {
	unsigned long s = entry_total_size(e);
	if (s >= threshold && s < *min) {
		*min = s;
	}
	for (int i = 0; i < e->file_count; i++) {
		part_2_aux(e->files[i], threshold, min);
	}
}

void part_2(const struct entry *root) {
	unsigned long occupied = entry_total_size(root);
	unsigned long need_to_free_at_least =
		occupied - (PART_2_TOTAL_SPACE - PART_2_REQUIRED_SPACE);
	unsigned long min = ULONG_MAX;
	part_2_aux(root, need_to_free_at_least, &min);
	printf("Part 2: %zu\n", min);
}

int main() {
	struct entry *root = parse(stdin);
	// entry_print(root, stdout, 0);
	part_1(root);
	part_2(root);
	entry_free(root);
	return 0;
}
