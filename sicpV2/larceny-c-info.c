#include <Windows.h>
#include <stdio.h>
int main(int argc, char **argv) {
   printf("\n(\n");
{   printf("%lu ",((unsigned long)(sizeof(SYSTEMTIME))));}
{ SYSTEMTIME s;  printf("%ld ",((long)((char *)&s.wMilliseconds - (char *)&s)));}
{ SYSTEMTIME s;  printf("%lu ",((unsigned long)(sizeof( s.wMilliseconds))));}
{ SYSTEMTIME s;  printf("%ld ",((long)((char *)&s.wSecond - (char *)&s)));}
{ SYSTEMTIME s;  printf("%lu ",((unsigned long)(sizeof( s.wSecond))));}
{ SYSTEMTIME s;  printf("%ld ",((long)((char *)&s.wMinute - (char *)&s)));}
{ SYSTEMTIME s;  printf("%lu ",((unsigned long)(sizeof( s.wMinute))));}
{ SYSTEMTIME s;  printf("%ld ",((long)((char *)&s.wHour - (char *)&s)));}
{ SYSTEMTIME s;  printf("%lu ",((unsigned long)(sizeof( s.wHour))));}
{ SYSTEMTIME s;  printf("%ld ",((long)((char *)&s.wDay - (char *)&s)));}
{ SYSTEMTIME s;  printf("%lu ",((unsigned long)(sizeof( s.wDay))));}
{ SYSTEMTIME s;  printf("%ld ",((long)((char *)&s.wDayOfWeek - (char *)&s)));}
{ SYSTEMTIME s;  printf("%lu ",((unsigned long)(sizeof( s.wDayOfWeek))));}
{ SYSTEMTIME s;  printf("%ld ",((long)((char *)&s.wMonth - (char *)&s)));}
{ SYSTEMTIME s;  printf("%lu ",((unsigned long)(sizeof( s.wMonth))));}
{ SYSTEMTIME s;  printf("%ld ",((long)((char *)&s.wYear - (char *)&s)));}
{ SYSTEMTIME s;  printf("%lu ",((unsigned long)(sizeof( s.wYear))));}
  printf("\n)\n");
  return 0;
}
