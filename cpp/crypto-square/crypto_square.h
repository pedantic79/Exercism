#if !defined(CRYPTO_SQUARE_H)
#define CRYPTO_SQUARE_H
#include <string>
#include <vector>

namespace crypto_square {
class cipher {
    std::string plain_;

  public:
    explicit cipher(const std::string &);
    const std::string &normalize_plain_text() const { return plain_; }
    std::vector<std::string> plain_text_segments() const;
    std::string normalized_cipher_text(bool normalize = true) const;
    std::string cipher_text() const;
};

} // namespace crypto_square

#endif // CRYPTO_SQUARE_H
