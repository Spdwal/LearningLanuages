#include<thread>
#include<iostream>

void func1(){
    std::cout << "func1" << std::endl;
}

void func2(){

    std::cout << "func2" << std::endl;
}


int main(){
    std::thread t1(func1);
    std::thread t2 = std::move(t1);
    t1 = std::move(std::thread(func2));
    std::thread t3;
    t3 = std::move(t2);
    t1 = std::move(t3);
}
