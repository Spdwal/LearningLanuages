#include <atomic>
#include <thread>
#include <assert.h>

std::atomic<bool> x, y;
std::atomic<int> z;
void write_x(){
    x.store(true, std::memory_order_release);
}

void write_y(){
    y.store(true, std::memory_order_release);
}

void read_x_then_y(){
    while(!x.load(std::memory_order_acquire));
    if(y.load(std::memory_order_acquire))
        ++z;
}

void read_y_then_x(){
    while(!y.load(std::memory_order_acquire));
    if(x.load(std::memory_order_acquire))
        ++z;
}


int main(){
    x = false;
    y = false;
    z = 0;
    std::thread a(write_x);
    std::thread b(write_y);
    std::thread c(read_x_then_y);
    std::thread d(read_y_then_x);
    a.join();
    b.join();
    c.join();
    d.join();

    //可能会发生，因为可能子啊加载x和y的时候，读取到的都是false；
    //因为x和y是由不同的线程写入，所以序列中每一次释放到获取都不会影响到其他线程的操作。
    //同步仅仅发生在
    assert(z.load() != 0);

    return 0;


}
