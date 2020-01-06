int C = 10;

int add(int x, int y)
{
	if (y == 2) {
		int x = 0;
		return x + y + C;
	}
	else { return y + x; }
}

int main()
{
	int x = 5;
	return add(x, 2);
}