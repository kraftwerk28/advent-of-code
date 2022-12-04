#include <stdlib.h>
#include <strings.h>

static int regs[4];
static int input[14];

int run_program() {
	bzero(regs, sizeof(regs));
#define w 0
#define x 1
#define y 2
#define z 3
#define inp(a) regs[a] = input[input_idx++];
#define add(a, b) regs[a] += b;
#define mul(a, b) regs[a] *= b;
#define div(a, b) regs[a] /= b;
#define mod(a, b) regs[a] %= b;
#define eql(a, b) regs[a] = regs[a] == b;
	int input_idx = 0;
	{
		// TODO: replace with macros
		// inp regs[w]
		// mul x 0
		// add x regs[z]
		// mod x 26
		// div z 1
		// add x 10
		// eql x regs[w]
		// eql x 0
		// mul y 0
		// add y 25
		// mul y regs[x]
		// add y 1
		// mul z regs[y]
		// mul y 0
		// add y regs[w]
		// add y 2
		// mul y regs[x]
		// add z regs[y]
		// inp regs[w]
		// mul x 0
		// add x regs[z]
		// mod x 26
		// div z 1
		// add x 14
		// eql x regs[w]
		// eql x 0
		// mul y 0
		// add y 25
		// mul y regs[x]
		// add y 1
		// mul z regs[y]
		// mul y 0
		// add y regs[w]
		// add y 13
		// mul y regs[x]
		// add z regs[y]
		// inp regs[w]
		// mul x 0
		// add x regs[z]
		// mod x 26
		// div z 1
		// add x 14
		// eql x regs[w]
		// eql x 0
		// mul y 0
		// add y 25
		// mul y regs[x]
		// add y 1
		// mul z regs[y]
		// mul y 0
		// add y regs[w]
		// add y 13
		// mul y regs[x]
		// add z regs[y]
		// inp regs[w]
		// mul x 0
		// add x regs[z]
		// mod x 26
		// div z 26
		// add x -13
		// eql x regs[w]
		// eql x 0
		// mul y 0
		// add y 25
		// mul y regs[x]
		// add y 1
		// mul z regs[y]
		// mul y 0
		// add y regs[w]
		// add y 9
		// mul y regs[x]
		// add z regs[y]
		// inp regs[w]
		// mul x 0
		// add x regs[z]
		// mod x 26
		// div z 1
		// add x 10
		// eql x regs[w]
		// eql x 0
		// mul y 0
		// add y 25
		// mul y regs[x]
		// add y 1
		// mul z regs[y]
		// mul y 0
		// add y regs[w]
		// add y 15
		// mul y regs[x]
		// add z regs[y]
		// inp regs[w]
		// mul x 0
		// add x regs[z]
		// mod x 26
		// div z 26
		// add x -13
		// eql x regs[w]
		// eql x 0
		// mul y 0
		// add y 25
		// mul y regs[x]
		// add y 1
		// mul z regs[y]
		// mul y 0
		// add y regs[w]
		// add y 3
		// mul y regs[x]
		// add z regs[y]
		// inp regs[w]
		// mul x 0
		// add x regs[z]
		// mod x 26
		// div z 26
		// add x -7
		// eql x regs[w]
		// eql x 0
		// mul y 0
		// add y 25
		// mul y regs[x]
		// add y 1
		// mul z regs[y]
		// mul y 0
		// add y regs[w]
		// add y 6
		// mul y regs[x]
		// add z regs[y]
		// inp regs[w]
		// mul x 0
		// add x regs[z]
		// mod x 26
		// div z 1
		// add x 11
		// eql x regs[w]
		// eql x 0
		// mul y 0
		// add y 25
		// mul y regs[x]
		// add y 1
		// mul z regs[y]
		// mul y 0
		// add y regs[w]
		// add y 5
		// mul y regs[x]
		// add z regs[y]
		// inp regs[w]
		// mul x 0
		// add x regs[z]
		// mod x 26
		// div z 1
		// add x 10
		// eql x regs[w]
		// eql x 0
		// mul y 0
		// add y 25
		// mul y regs[x]
		// add y 1
		// mul z regs[y]
		// mul y 0
		// add y regs[w]
		// add y 16
		// mul y regs[x]
		// add z regs[y]
		// inp regs[w]
		// mul x 0
		// add x regs[z]
		// mod x 26
		// div z 1
		// add x 13
		// eql x regs[w]
		// eql x 0
		// mul y 0
		// add y 25
		// mul y regs[x]
		// add y 1
		// mul z regs[y]
		// mul y 0
		// add y regs[w]
		// add y 1
		// mul y regs[x]
		// add z regs[y]
		// inp regs[w]
		// mul x 0
		// add x regs[z]
		// mod x 26
		// div z 26
		// add x -4
		// eql x regs[w]
		// eql x 0
		// mul y 0
		// add y 25
		// mul y regs[x]
		// add y 1
		// mul z regs[y]
		// mul y 0
		// add y regs[w]
		// add y 6
		// mul y regs[x]
		// add z regs[y]
		// inp regs[w]
		// mul x 0
		// add x regs[z]
		// mod x 26
		// div z 26
		// add x -9
		// eql x regs[w]
		// eql x 0
		// mul y 0
		// add y 25
		// mul y regs[x]
		// add y 1
		// mul z regs[y]
		// mul y 0
		// add y regs[w]
		// add y 3
		// mul y regs[x]
		// add z regs[y]
		// inp regs[w]
		// mul x 0
		// add x regs[z]
		// mod x 26
		// div z 26
		// add x -13
		// eql x regs[w]
		// eql x 0
		// mul y 0
		// add y 25
		// mul y regs[x]
		// add y 1
		// mul z regs[y]
		// mul y 0
		// add y regs[w]
		// add y 7
		// mul y regs[x]
		// add z regs[y]
		// inp regs[w]
		// mul x 0
		// add x regs[z]
		// mod x 26
		// div z 26
		// add x -9
		// eql x regs[w]
		// eql x 0
		// mul y 0
		// add y 25
		// mul y regs[x]
		// add y 1
		// mul z regs[y]
		// mul y 0
		// add y regs[w]
		// add y 9
		// mul y regs[x]
		// add z regs[y]
	}
	return 0;
}

int main(int argc, char *argv[]) {
	return EXIT_SUCCESS;
}
