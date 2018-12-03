#include <atomic>
#include <thread>
#include <assert.h>

std::atomic<bool> x, y;
std::atomic<int> z;

void write_x(){
    x.store(true, std::memory_order_seq_cst);
}

void write_y(){
    y.store(true, std::memory_order_seq_cst);
}

void read_x_then_y(){
    while(!x.load(std::memory_order_seq_cst));
    if(y.load(std::memory_order_seq_cst))
        ++z;
}

void read_y_then_x(){
    while(!y.load(std::memory_order_seq_cst));
    if(x.load(std::memory_order_seq_cst))
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
    //如果先是x为true，而y一直到read_x_then_y的if处还为false；
    //那么就表示时read_y_then_x还在被阻塞，然后等待y为true，此时x，y都为true；
    //此时read_y_then_x使的z++；
    //反之亦然。
    //或者在c d线程阻塞同时，xy都为true，则此时z为2;

    a.join();
    b.join();
    c.join();
    d.join();

    //此语句永远不会触发，为不是存储x的操做发生，就是存储y的操作发生。
    assert(z.load() != 0);
    return 0;
}
