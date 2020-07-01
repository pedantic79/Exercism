#if !defined(BINARY_SEARCH_TREE_H)
#define BINARY_SEARCH_TREE_H
#include <iostream>
#include <memory>

namespace binary_search_tree {
template <typename T> class binary_tree {
  public:
    explicit binary_tree(T item) : value_(item), size_(1) {}

    void insert(T item) {
        if (item <= value_) {
            if (left_) {
                left_->insert(item);
            } else {
                left_ = std::make_unique<binary_tree<T>>(item);
            }
        } else {
            if (right_) {
                right_->insert(item);
            } else {
                right_ = std::make_unique<binary_tree<T>>(item);
            }
        }
        size_++;
    }

    const std::unique_ptr<binary_tree<T>> &left() const { return left_; }
    const std::unique_ptr<binary_tree<T>> &right() const { return right_; }
    T data() const { return value_; }

    struct iterator {
        using const_ref = const T &;
        using difference_type = std::ptrdiff_t;
        using iterator_category = std::input_iterator_tag;
        using pointer = binary_tree<T> *;
        using value_type = binary_tree<T>;

        const T &operator*() const { return tree_->at(pos_); }
        bool operator!=(const iterator &other) const {
            return pos_ != other.pos_;
        };
        iterator &operator++() {
            pos_++;
            return *this;
        }

      private:
        explicit iterator(const binary_tree *t, size_t pos)
            : tree_(t), pos_(pos){};
        const binary_tree *tree_;
        size_t pos_;
        friend class binary_tree;
    };

    const iterator begin() { return iterator(this, 0); }
    const iterator end() { return iterator(this, size_); }

  private:
    T value_;
    std::unique_ptr<binary_tree<T>> left_;
    std::unique_ptr<binary_tree<T>> right_;
    size_t size_;

    const T &at(size_t pos) const {
        size_t size = left_ ? left_->size_ : 0;
        if (pos < size) {
            return left_->at(pos);
        }

        if (pos == size) {
            return value_;
        }

        return right_->at(pos - size - 1);
    }
}; // namespace binary_search_tree

} // namespace binary_search_tree

#endif // BINARY_SEARCH_TREE_H
