#if !defined(BINARY_SEARCH_TREE_H)
#define BINARY_SEARCH_TREE_H
#include <memory>
#include <stack>

namespace binary_search_tree {
template <typename T> class binary_tree {
  public:
    explicit binary_tree(T item) : value_(item) {}

    void insert(T item) {
        if (item <= value_) {
            if (left_) {
                return left_->insert(item);
            } else {
                left_ = std::make_unique<binary_tree<T>>(item);
            }
        } else {
            if (right_) {
                return right_->insert(item);
            } else {
                right_ = std::make_unique<binary_tree<T>>(item);
            }
        }
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

        const_ref operator*() const { return stack_.top()->value_; }

        bool operator!=(const iterator &other) const {
            return stack_ != other.stack_;
        };

        iterator &operator++() {
            auto node = stack_.top()->right_.get();
            stack_.pop();
            while (node) {
                stack_.push(node);
                node = node->left_.get();
            }
            return *this;
        }

      private:
        iterator() = default;
        explicit iterator(const pointer t) {
            for (auto node = t; node; node = node->left_.get())
                stack_.push(node);
        }

        std::stack<pointer> stack_;
        friend class binary_tree;
    };

    const iterator begin() { return iterator(this); }
    const iterator end() { return {}; }

  private:
    T value_;
    std::unique_ptr<binary_tree<T>> left_;
    std::unique_ptr<binary_tree<T>> right_;
}; // namespace binary_search_tree

} // namespace binary_search_tree

#endif // BINARY_SEARCH_TREE_H
