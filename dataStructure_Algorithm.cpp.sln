C:\Users\sooyeon Kang\source\repos



/*
#include <iostream>

using namespace std;

int main()
{
	cout << "Hello World!" << endl;
	return 0;
}
*/

// Windows c file 경로 탐색
/*
#include <iostream>
#include <Windows.h>
#include <atlconv.h>

int main()
{
	wchar_t path[MAX_PATH] = { 0 };
	GetModuleFileName(NULL, path, MAX_PATH);

	USES_CONVERSION;
	std::string str = W2A(path);
	str = str.substr(0, str.find_last_of("\\/"));

	printf("%s\n", str.c_str());
}
	// C:\Users\sooyeon Kang\source\repos\dataStructure_Algorithm.cpp\x64\Debug
	*/


// & Ampersand Operato 주소값
/*
#include <cstdio>

int main(void)
{
	char c = "A";
		// const char* 형식의 값을 사용하여 char 형식의 엔터티를 초기화할 수 없음
	char* pc = &c;

	printf("%c %p\n", c, pc);
	printf("%p %p\n", &c, &pc);
	printf("%d %d\n", sizeof(c), sizeof(pc));
}
*/


/*
#include <cstdio>

int main()
{
	int a, b, c;
	int* pa = &a, * pb = &b, * pc = &c;
		// call by reference : passing addresses
		// != call by value : passing values

	*pa = 10, * pb = 20;
	*pc = *pa + *pb;

	printf("%d %d %d", a, b, c);

	return 0;
}
*/


// stack & queue_Standard Template Library
/* Error
#include <cstdio>
# include <cstring>
# include <stack>
# include <queue>

bool Check_Palindrome(const char* _str)
{
	std::stack<char> s;
	std::queue<char> q;

	int len = strlen( _str );
	
	for (int i = 0; i < len; i++)
	{
		s.push( _str[i] );
		q.push( _str[i] );
	}

	while (!s.empty())
	{
		if (s.top() != q.front())
			return false;
		s.pop();
		q.pop();
	}
	return true;
}
*/


/*
#include <iostream>
#include <stack>
	// 순서가 별로 중요하지 않은 경우 활용
	// 괄호 매칭 알고리즘

using namespace std;
	// 사용하겠다, 이름 공간에 있는 std(클래스)에 정의되어 있는 함수들

int main(void) {
	stack<int> s;
		// 스택 생성
	cout << s.empty() << endl;
		// 초기 스택은 비어있으므로 1 출력
		//(스택이 비어있는지를 확인 후 true,false 값을 반환)
	s.push(4);
		// 4 push
	s.push(2);
		// 2 push
	s.push(3);
		// 3 push

	cout << s.top() << endl;
		// 가장 위에 있는 값을 반환
	s.pop();
		// 가장 위에 있는 값을 삭제

	cout << s.empty() << endl;
	cout << s.size() << endl;
		// 스택의 사이즈 반환

	return 0;
}
*/

/*
#include <iostream>
#include <queue>
using namespace std;

int main(void) {
	queue<int> q;
	// 큐 생성

	q.push(4);
	// 4 push
	q.push(2);
	// 2 push
	q.push(3);
	// 3 push

	q.pop(); 
		// 가장 앞에 있는 4 삭제

	cout << q.front() << endl;
	// 가장 앞에 있는 값을 반환
	cout << q.back() << endl;
	// 가장 뒤에 있는 값을 반환

	cout << q.empty() << endl;
	cout << q.size() << endl;
	// 큐의 사이즈 반환

	return 0;
}
*/


// ndarray int i[2][3]; or int i[1][2][3]
/*
#include <stdio.h>

int main() {
	
	int array[2][3][4];
		// 3차원 배열의 초기화와 논리적 구조
	int i, j, k, value = 1;

	for (i = 0; i < 2; i++) {
		for (j = 0; j < 3; j++) {
			for (k = 0; k < 4; k++) {
				array[i][j][k] = value;
				printf("\n array[%d][%d][%d] = %d", i, j, k, array[i][j][k]);
				value++;
			}
		}
	}
	getchar();
}
*/

/* return
 array[0][0][0] = 1
 array[0][0][1] = 2
 array[0][0][2] = 3
 array[0][0][3] = 4
 array[0][1][0] = 5
 array[0][1][1] = 6
 array[0][1][2] = 7
 array[0][1][3] = 8
 array[0][2][0] = 9
 array[0][2][1] = 10
 array[0][2][2] = 11
 array[0][2][3] = 12
 array[1][0][0] = 13
 array[1][0][1] = 14
 array[1][0][2] = 15
 array[1][0][3] = 16
 array[1][1][0] = 17
 array[1][1][1] = 18
 array[1][1][2] = 19
 array[1][1][3] = 20
 array[1][2][0] = 21
 array[1][2][1] = 22
 array[1][2][2] = 23
 array[1][2][3] = 24
 */


// ndarray char c[2][3][20]; 1차원 문자 배열을 2개(학생 1,2)를 묶어 3개 문자(이름, 학과, 학번)까지 저장하고, 2차원 문자 배열을 3개 묶어 20개 문자를 저장하는 배열
/*
#include <stdio.h>

int main() {
	int i, j, k;
	char student[2][3][20];

	for (i = 0; i < 2; i++) {
		printf("\n 학생 %d의 이름 :", i + 1);
		gets(student[i][0]); 
			// error
		printf("\n 학생 %d의 학과 :", i + 1);
		gets(student[i][1]);
		printf("\n 학생 %d의 학번 :", i + 1);
		gets(student[i][2]);
	}

	// 3차원 배열을 이용한 문자배열 저장하고, 배열 내용 출력 프로그램
	for (i = 0; i < 2; i++) {
		printf("\n\n 학생의 %d", i + 1);
		for (j = 0; j < 3; j++) {
			printf("\n\t");
			
			for (k = 0; student[i][j][k] != "\0"; k++) {
					// error
				printf("%c", student[i][j][k]);
			}
		}
	}
	getchar();
}
*/


// pointer 주소연산자 & 와 참조 연산자 * :  %u ?
// 포인터 = &변수

// 변수 = *포인터;
// *포인터 = 값;

// 포인터 연산자를 이용한 변수 액세스 프로그램

# include <stdio.h>

int main() {
	
	int i = 10, j = 20;

	int* ptr;
		// STEP 1 : 주소 연산자를 이용하여 변수 I의 주소를 포인터 ptr에 할당한다. 포인터 ptr은 변수 i를 가리킨다.

	printf("\n i의 값 = %d \n j의 값 = %d", i, j);
	printf("\n i의 메모리 주소( &i ) = %u", &i);
	printf("\n j의 메모리 주소( &j ) = %u", &j);

	ptr = &i;
		// i, 참조 연산자
		// 참조 연산자를 이용하여 포인터 ptr이 가리키는 영역에 값 10을 지정한다. 따라서 변수 i에 10이 저장된다.

	printf("\n\n << ptr = &i 실행 >>");
	printf("\n ptr의 메모리 주소( &ptr ) = %u", &ptr);
	printf("\n ptr의 값( ptr ) = %u", ptr);
	printf("\n ptr의 참조값( *ptr ) = %d", *ptr);

	ptr = &j;
		// j, 참조 연산자

	printf("\n\n << ptr = &j 실행 >>");
	printf("\n ptr의 메모리 주소( &ptr ) = %u", &ptr);
	printf("\n ptr의 값( ptr ) = %u", ptr);
	printf("\n ptr의 참조값( *ptr ) = %d", *ptr);

	i = *ptr;
		// 다시 참조 연산자를 사용하여 ptr이 가리키는 영역의 값을 변수 i에 지정한다. 따라서 ptr이 가리키는 변수 j의 값인 20을 변수 i에 저장한다.


	printf("\n\n <<i = *ptr 실행>>");
	printf("\n i의 값 = %d", i);

	getchar();
}

