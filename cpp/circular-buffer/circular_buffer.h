#ifndef CIRCULAR_BUFFER_H
#define CIRCULAR_BUFFER_H
#include <memory>

namespace circular_buffer {

template <class T> class circular_buffer {
  public:
    explicit circular_buffer(int capacity) : capacity_(capacity) {
        buf_ = std::make_unique<T[]>(capacity);
        clear();
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
        advance_write();
    }

    void overwrite(T item) {
        if (size_ >= capacity_) {
            advance_read();
        }

        write(item);
    }

    T read() {
        if (size_ == 0) {
            throw std::domain_error("buffer empty");
        }

        T item = buf_[read_];
        advance_read();
        return item;
    }

  private:
    std::unique_ptr<T[]> buf_;
    size_t capacity_;
    size_t read_;
    size_t write_;
    size_t size_;

    void advance_read() {
        ++read_ %= capacity_;
        --size_;
    }

    void advance_write() {
        ++write_ %= capacity_;
        ++size_;
    }
};

} // namespace circular_buffer

#endif // CIRCULAR_BUFFER_H
