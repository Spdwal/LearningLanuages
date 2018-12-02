#include <deque>
#include <mutex>
#include <future>
#include <thread>
#include <utility>


std::mutex m;
std::deque<std::packaged_task<void()>> tasks;

bool gui_shutdown_messagge_receiced();
void get_and_process_gui_message();

void gui_thread(){
    while(!gui_shutdown_messagge_receiced()){
        //轮询
        get_and_process_gui_message();
        std::packaged_task<void()> task;

        std::lock_guard<std::mutex> lk(m);
        if(tasks.empty()){
            continue;
        }

        task = std::move(tasks.front());
        tasks.pop_front();
        //调用传入的函数，导致使post_task_for_gui_thread的future获得返回值。
        task();
    }
}


std::thread gui_bg_thread(gui_thread);

template<typename Func>
std::future<void> post_task_for_gui_thread(Func f)
{
    std::packaged_task<void()> task(f);
    std::future<void> res=task.get_future();
    std::lock_guard<std::mutex> lk(m);
    //给异步线程传入一个任务，然后让gui_thread进行调用
    tasks.push_back(std::move(task));
    return res;
}
