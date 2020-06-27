#ifndef CIRCULAR_BUFFER_H
#define CIRCULAR_BUFFER_H
#include <memory>

namespace circular_buffer {

template <class T> class circular_buffer {
  private:
    std::unique_ptr<T[]> buf_;
    size_t capacity_;
    size_t read_;
    size_t write_;
    size_t size_;

  public:
    explicit circular_buffer(int capacity)
        : capacity_(capacity), read_(0), write_(0), size_(0) {
        buf_ = std::make_unique<T[]>(capacity);
    }

    void clear() {
        read_ = 0;
        write_ = 0;
        size_ = 0;
    }

    void write(T item) {
        if (size_ == capacity_) {
            throw std::domain_error("buffer full");
        }

        buf_[write_] = item;
        ++write_ %= capacity_;
        ++size_;
    }

    void overwrite(T item) {
        if (size_ >= capacity_) {
            read();
        }

        write(item);
    }

    T read() {
        if (size_ == 0) {
            throw std::domain_error("buffer empty");
        }

        T item = buf_[read_];
        ++read_ %= capacity_;
        --size_;

        return item;
    }
};

} // namespace circular_buffer

#endif // CIRCULAR_BUFFER_H
