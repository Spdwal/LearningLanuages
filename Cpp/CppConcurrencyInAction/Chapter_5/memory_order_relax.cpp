#include <atomic>
#include <thread>
#include <assert.h>

std::atomic<bool> x, y;
std::atomic<int> z;

void write_x_then_y(){
    x.store(true, std::memory_order_relaxed);
    y.store(true, std::memory_order_relaxed);
}

void read_y_then_x(){
    while(!y.load(std::memory_order_relaxed));
    if(x.load(std::memory_order_relaxed))
        ++z;
}

int main(){
    x = false;
    y = false;
    z = 0;
    std::thread a(write_x_then_y);
    std::thread b(read_y_then_x);
    a.join();
    b.join();
    //此处可能会触发，因为加载x的操作可能读到false，即使加载y的操作读取到true
    //并且存储x先发与存储y的操作，x和y是两个不同的变量，以这里没有顺序去保证每个操作产生相关值的可见性
    assert(z.load() != 0);

    return 0;
}
