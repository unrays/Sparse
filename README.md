*Hello, this is my third and first successful attempt at coding and designing a small sparse set. On Sunday, November 30, 2025, I challenged myself yesterday to learn how to design and code a sparse set from scratch, with minimal documentation. A bit of a "from scratch" approach, but we'll guide you. I'm not reinventing the concept; the sparse set is a very popular and powerful algorithm. I've simply imagined my own version of it. It probably resembles other implementations given the uniqueness of the algorithm and the system; there aren't a hundred different ways to implement it. Basically, I made two versions before arriving at this one. The first consisted of a Dense class and a Sparse class that communicated with each other. This isn't the best idea, although valid; it breaks the concept itself in such a way that it literally forces you to zigzag with the data between the two classes. I don't need to tell you that this is bad, especially for an ECS system. The second one was very similar, in the same vein, we'll say. It was noticeably more complex and included SFINAE and Variadic logic, just enough to make any template meta-programming wizard reading my code do backflips :)*

*More seriously, these iterations were good, but from a performance and conceptual standpoint, they weren't quite right. So, this evening, around 7 PM (an hour and a half ago), I decided to redo everything, but this time trying to have as much locality of code and information as possible. As a result, you can take a look at it; I worked from scratch, rewriting all the logic and ensuring the highest possible code quality. Also, during this project, I learned a lot about "pro" class and object design, such as the rule of 5 (or 7, it seems to increase exponentially over the years lmao) and about function overloading for r and l values ​​(especially by combining them with template<typename U> and using good old std::forward<U>(arg)). Anyway, I've learned quite a bit about the subject, I'm super happy with the result, especially since algorithms are my weakness and I'm really, really bad at it. I still managed to implement a sparse set and I'll be able to show it to my friends tomorrow :)*

*Finally, I intend to implement this code as the foundation of my upcoming large ECS project, "Exotic," and I wanted to post it here for archiving purposes. It would be a shame to lose such an important piece of my learning within the enormous codebase that this project will be. Thank you and have a good day.*

*Moreover, this is the final project that concludes November, which was one of the most important months of my young programming career. During this month, I went from being an amateur C# programmer who understood absolutely nothing about optimization, design, hardcore C++ code, and above all, my beloved meta-programming template. I've successfully completed numerous projects like Apex, Harmony, Lumen, Catalyst, Wave, Exotic, and Sparse, which is the final project I've undertaken :) I've learned many other things such as sfinae, move semantics, smart pointers and how they work, the hardcore meta-programming template that I absolutely love, and many extremely important concepts about C++ and its best practices (just look at the code from a month ago; the difference is night and day). Now, let's say hello to December! Who knows what projects await me this month! In the meantime, I wish you a wonderful rest of the day and stay tuned for the new projects I'll be releasing :)*

```cpp
// Copyright (c) November 2025 Félix-Olivier Dumas. All rights reserved.
// Licensed under the terms described in the LICENSE file.

template<typename T>
struct Sparse {
private:
    static constexpr std::size_t DEFAULT_DENSE_CAPACITY = 2048;
    static constexpr std::size_t DEFAULT_SPARSE_CAPACITY = 16384;

    inline void error_not_enough_capacity(const std::string& context, size_t required, size_t actual) {
        std::cerr << "[ERROR] " << context
                  << " | Required size: " << required
                  << ", Actual size: " << actual << std::endl;
    }

    inline void error_entity_has_no_component(std::size_t entity_id) {
        std::cerr << "[ERROR] Entity " << entity_id
                  << " does not have a component!" << std::endl;
    }

    inline void error_entity_already_has_component(std::size_t entity_id) {
        std::cerr << "[ERROR] Entity " << entity_id
            << " already has a component!" << std::endl;
    }

    inline constexpr bool is_valid_entity_id(std::size_t entity_id) const noexcept {
        return entity_id < sparse_.size();
    }

public:
    Sparse(std::size_t init_dense_capacity = DEFAULT_DENSE_CAPACITY,
        std::size_t init_sparse_capacity = DEFAULT_SPARSE_CAPACITY) {
        dense_.reserve(init_dense_capacity);
        reverse_.reserve(init_dense_capacity);
        sparse_.reserve(init_sparse_capacity);
        sparse_.resize(init_sparse_capacity, SIZE_MAX);
    }

    Sparse(const Sparse&) = default;
    Sparse& operator=(const Sparse&) = default;
    Sparse(Sparse&&) noexcept = default;
    Sparse& operator=(Sparse&&) noexcept = default;
    ~Sparse() = default;

public:
    template<typename U>
    void insert(std::size_t entity_id, U&& component) noexcept {
        if (!is_valid_entity_id(entity_id)) {
            error_not_enough_capacity(
                "Sparse vector too small",
                entity_id + 1,
                sparse_.size()
            ); return;
        }

        if (contains(entity_id)) {
            error_entity_already_has_component(entity_id);
            return;
        }

        dense_.push_back(std::forward<U>(component));
        std::size_t component_index = dense_.size() - 1;

        sparse_[entity_id] = component_index;
        reverse_.push_back(entity_id);
    }

    template<typename... Ts, typename U>
    auto batch_insert(Ts... entity_ids, const U& component) noexcept ->
        std::enable_if_t<(is_index_type_v<Ts> && ...) && (sizeof...(Ts) > 0),
    void> { (insert(entity_ids, component), ...); }

    auto emplace_default(std::size_t entity_id) noexcept ->
    std::enable_if_t<std::is_default_constructible_v<T>, void> {
        if (!is_valid_entity_id(entity_id)) {
            error_not_enough_capacity(
                "Sparse vector too small",
                entity_id + 1,
                sparse_.size()
            ); return;
        }

        if (contains(entity_id)) {
            error_entity_already_has_component(entity_id);
            return;
        }

        dense_.emplace_back();
        std::size_t component_index = dense_.size() - 1;

        sparse_[entity_id] = component_index;
        reverse_.push_back(entity_id);
    }

    template<typename... Ts>
    auto batch_emplace(Ts... entity_ids) noexcept -> 
        std::enable_if_t<std::is_default_constructible_v<T>
        && (is_index_type_v<Ts> && ...) && (sizeof...(Ts) > 0),        
    void> { (emplace_default(entity_ids), ...); }

    void remove_swap(std::size_t entity_id) {
        if (!is_valid_entity_id(entity_id)) {
            error_not_enough_capacity(
                "Sparse vector too small",
                entity_id + 1,
                sparse_.size()
            ); return;
        }

        if (!contains(entity_id)) {
            error_entity_has_no_component(entity_id);
            return;
        }
           
        std::size_t component_index = sparse_[entity_id];
        std::size_t last_component_index = dense_.size() - 1;
        std::size_t last_entity_id = reverse_[last_component_index];

        std::swap(dense_[component_index], dense_[dense_.size() - 1]);
        dense_.pop_back();

        std::swap(reverse_[component_index], reverse_[last_component_index]);
        reverse_.pop_back();

        sparse_[last_entity_id] = component_index;
        sparse_[entity_id] = SIZE_MAX;
    }

    template<typename... Ts>
    auto batch_remove_swap(Ts... entity_ids) noexcept ->
        std::enable_if_t<((is_index_type_v<Ts>) && ...)
        && (sizeof...(Ts) > 0), void> { (remove_swap(entity_ids), ...); }

public:
    bool contains(std::size_t entity_id) const {
        return is_valid_entity_id(entity_id) && sparse_[entity_id] != SIZE_MAX;
    }

    template<typename... Ts>
    auto batch_contains(Ts... entity_ids) const noexcept ->
        std::enable_if_t<(is_index_type_v<Ts> && ...)
        && (sizeof...(Ts) > 0), bool> { return (contains(entity_ids) && ...); }

    std::size_t count() const noexcept { return dense_.size(); }
    std::size_t capacity() const noexcept { return sparse_.capacity(); }
    std::size_t empty() const noexcept { return sparse_.empty(); }

    void clear_sparse() noexcept { sparse_.clear(); }
    void clear_dense() noexcept { dense_.clear(); }
    void clear_binding() noexcept { reverse_.clear(); }

    void reserve(std::size_t new_capacity) noexcept { sparse_.reserve(new_capacity); }

    void shrink_sparse_to_fit() noexcept { sparse_.shrink_to_fit(); }
    void shrink_dense_to_fit() noexcept { dense_.shrink_to_fit(); }
    void shrink_binding_to_fit() noexcept { reverse_.shrink_to_fit(); }

public:
    auto begin() noexcept { return dense_.begin(); }
    auto end() noexcept { return dense_.end(); }
    auto begin() const noexcept { return dense_.begin(); }
    auto end() const noexcept { return dense_.end(); }

public:
    std::optional<T*> operator[](std::size_t entity_id) noexcept {
        if (!is_valid_entity_id(entity_id) || sparse_[entity_id] == SIZE_MAX) {
            error_not_enough_capacity(
                "Sparse vector too small or entity has no component",
                entity_id + 1,
                sparse_.size()
            );
            return std::nullopt;
        }
        return &dense_[sparse_[entity_id]];
    }

    const std::optional<T*> operator[](std::size_t entity_id) const noexcept {
        if (!is_valid_entity_id(entity_id) || sparse_[entity_id] == SIZE_MAX) {
            error_not_enough_capacity(
                "Sparse vector too small or entity has no component",
                entity_id + 1,
                sparse_.size()
            );
            return std::nullopt;
        }
        return &dense_[sparse_[entity_id]];
    }

    std::optional<T*> get(std::size_t entity_id) & noexcept {
        if (!is_valid_entity_id(entity_id) || sparse_[entity_id] == SIZE_MAX) {
            error_not_enough_capacity(
                "Sparse vector too small or entity has no component",
                entity_id + 1,
                sparse_.size()
            );
            return std::nullopt;
        }
        return &dense_[sparse_[entity_id]];
    }

    std::optional<const T*> get(std::size_t entity_id) const & noexcept {
        if (!is_valid_entity_id(entity_id) || sparse_[entity_id] == SIZE_MAX) {
            error_not_enough_capacity(
                "Sparse vector too small or entity has no component",
                entity_id + 1,
                sparse_.size()
            );
            return std::nullopt;
        }
        return &dense_[sparse_[entity_id]];
    }

private:
    std::vector<std::size_t> sparse_; // contient un entity_id -> component_id
    std::vector<std::size_t> reverse_; // contient un composante_id -> entity_id
    std::vector<T> dense_;  //contient un component_id -> component (T)
};
```

```cpp
// Copyright (c) November 2025 Félix-Olivier Dumas. All rights reserved.
// Licensed under the terms described in the LICENSE file.

template<typename T>
struct is_index_type {
    static constexpr bool value =
        std::disjunction_v<
            std::is_same<T, std::size_t>,
            std::is_same<T, unsigned int>,
            std::is_same<T, unsigned long>,
            std::is_same<T, unsigned long long>
        >;
}; template<typename T>
inline constexpr bool is_index_type_v = is_index_type<T>::value;
```
