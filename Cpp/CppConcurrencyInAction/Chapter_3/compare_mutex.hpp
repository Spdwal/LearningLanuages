#include<mutex>


//此处的比较有一个问题就是，只是在获得lhs_value和rhs_value的那个时间点上的值相等，但是在之后是否相等，并不能保证。
class Y{
private:
    int some_detail;
    mutable std::mutex m;
    int get_detail() const{
        std::lock_guard<std::mutex> lock_a(m);
        return some_detail;
    }

public:
    Y(int sd):some_detail(sd){}

    friend bool operator==(Y const& lhs, Y const& rhs){
        if(&lhs == &rhs){
            return false;
        }

        int const lhs_value = lhs.get_detail();
        int const rhs_value = rhs.get_detail();
        return lhs_value == rhs_value;
    }
};
