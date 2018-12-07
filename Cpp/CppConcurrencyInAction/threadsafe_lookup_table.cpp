#include <list>
#include <utility>
#include <functional>
#include <shared_mutex>

template<typename Key, typename Value, typename Hash=std::hash<Key> >
class threadsafe_lockup_table{
private:
    class bucket_type{
    private:
        typedef std::pair<Key, Value> bucket_value;
        typedef std::list<bucket_value> bucket_data;
        typedef typename bucket_data::iterator bucket_iterator;

        bucket_data data;

        mutable std::shared_mutex mutex;

        bucket_iterator find_entry_for(Key const key) const{
            return std::find_if(data.begin(), data.end(), [&](bucket_value const& item){return item.firt==key;});
        }

    public:
        Value value_for(Key const& key, Value const& default_value){
            std::shared_lock<std::shared_mutex> lock(mutex);
            bucket_iterator const found_entry = find_entry_for(key);
            return (found_entry == data.end()) ? default_value : found_entry->second;
        }

        void add_or_update_mapping(Key const& key, Value const& value){
            std::unique_lock<std::shared_mutex> lock(mutex);
            bucket_iterator const found_entry = find_entry_for(key);
            if(found_entry == data.end()){
                data.push_back(bucket_value(key, value));
            }else{
                found_entry->second = value;
            }
        }

        void remove_mapping(Key const& key){
            std::unique_lock<std::shared_mutex> lock(mutex);
            bucket_iterator const found_entry = find_entry_for(key);
            if(found_entry != data.end()){
                data.earse(found_entry);
            }
        }
    };

    std::vector<std::unique_ptr<bucket_type>> buckets;
    Hash hasher;

    bucket_type& get_bucket(Key const& key) const{
        std::size_t const bucket_index=hasher(key) % buckets.size();
        return *buckets[bucket_index];
    }

public:
    typedef Key key_type;
    typedef Value mapped_type;
    typedef Hash hash_type;



};
