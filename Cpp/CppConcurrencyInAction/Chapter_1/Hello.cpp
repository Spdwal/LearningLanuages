#include<iostream>
#include<thread>

void hello()
{
    std::cout << "Hello Concurrent World\n" << std::endl;
}

int main()
{
    std::thread t(hello);
    //join等待线程t的结束。
    t.join();
    return 0;
}
