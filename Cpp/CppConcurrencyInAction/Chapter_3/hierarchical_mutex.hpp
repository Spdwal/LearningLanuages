#include<mutex>

class hierarchical_mutex{
    std::mutex internal_mutex;
    //保存了该锁的层次
    unsigned long const hierarchy_value;
    //这个是为了保存this_thread_hierarchy_value，方便在unlock的时候恢复它。
    unsigned long previous_hierarchy_value;

    static thread_local unsigned long this_thread_hierarchy_value;

    void check_for_hierarchy_violation(){
        if(this_thread_hierarchy_value <= hierarchy_value){
            throw std::logic_error("mutex hierarchy violated.\n");
        }
    }

    void update_hierarchy_value(){
        //保存之前的this_thread_hierarchy_value,方便以后恢复。
        previous_hierarchy_value = this_thread_hierarchy_value;
        //更新this_thread_hierarchy_value
        this_thread_hierarchy_value = hierarchy_value;
    }

public:
    explicit hierarchical_mutex(unsigned long value):hierarchy_value(value), previous_hierarchy_value(0){
    }

    void lock(){
        //此时如果层级比较低的锁，在这个check后会抛出逻辑异常，无法锁住。
        check_for_hierarchy_violation();
        internal_mutex.lock();
        //更新this_thread_hierarchy_value，使他变成自己这个锁的层级
        update_hierarchy_value();
    }

    void unlock(){
        //恢复this_thread_hierarchy_value
        this_thread_hierarchy_value = previous_hierarchy_value;
        internal_mutex.unlock();
    }

    bool try_lock(){
        check_for_hierarchy_violation();
        if(!internal_mutex.try_lock()){
            return false;
        }
        update_hierarchy_value();
        return true;
    }

};
//thread_local表示该对象只有在该线程内有用，跳出线程后释放线程，每一个线程都有一个该对象的实例。
//初始化为最大值，这是为了保证最初所有的线程都被锁定
thread_local unsigned long hierarchical_mutex::this_thread_hierarchy_value(ULONG_MAX);
