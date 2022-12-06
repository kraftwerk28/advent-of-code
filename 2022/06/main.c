#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define BUF_SIZE 4096
#define START_PACKET_LEN 4
#define START_MESSAGE_LEN 14

bool range_uniq(const char *buf, int start, int end) {
	for (int i = start; i < end; i++) {
		for (int j = i + 1; j < end; j++) {
			if (buf[i] == buf[j]) {
				return false;
			}
		}
	}
	return true;
}

void part_1(const char *input, int input_len) {
	for (int i = 0; i < input_len - START_PACKET_LEN + 1; i++) {
		if (range_uniq(input, i, i + START_PACKET_LEN)) {
			printf("Part 1: %d\n", i + START_PACKET_LEN);
			return;
		}
	}
}

void part_2(const char *input, int input_len) {
	for (int i = 0; i < input_len - START_MESSAGE_LEN + 1; i++) {
		if (range_uniq(input, i, i + START_MESSAGE_LEN)) {
			printf("Part 2: %d\n", i + START_MESSAGE_LEN);
			return;
		}
	}
}

int main(int argc, char *argv[]) {
	char *buf = calloc(BUF_SIZE, sizeof(char));
	int input_len;
	assert((input_len = read(0, buf, BUF_SIZE)) > 0);
	part_1(buf, input_len);
	part_2(buf, input_len);
	free(buf);
	return 0;
}
