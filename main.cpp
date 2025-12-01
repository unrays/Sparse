// Copyright (c) November 2025 Félix-Olivier Dumas. All rights reserved.
// Licensed under the terms described in the LICENSE file.

#pragma once
#include <iostream>
#include <vector>
#include <chrono>
#include <cstdint>
#include <stdexcept>
#include <type_traits>
#include <tuple>
#include <cassert>
#include <optional>


template<typename T>
class DenseArray {
public:
    DenseArray(std::size_t initCapacity = DEFAULT_CAPACITY) noexcept {
        dense_.reserve(initCapacity);
    }

    DenseArray(const DenseArray&) = default;
    DenseArray& operator=(const DenseArray&) = default;
    DenseArray(DenseArray&&) noexcept = default;
    DenseArray& operator=(DenseArray&&) noexcept = default;
    ~DenseArray() = default;

public:
    void push_back(T value) noexcept { dense_.push_back(value); }
    void push_back(T&& value) noexcept { dense_.push_back(std::move(value)); }

    template<typename U>
    void replace(std::size_t index, U&& value) noexcept {
        assert(index < dense_.size());
        dense_[index] = std::forward<U>(value);
    }

    auto emplace_back() ->
        std::enable_if_t<
        std::is_default_constructible_v<T>,
    void> { dense_.emplace_back(); }

    void swap_and_pop(std::size_t index) noexcept {
        std::swap(dense_[index], dense_[dense_.size() - 1]);
        dense_.pop_back();
    }

public:
    bool is_empty() const noexcept { return dense_.empty(); }

    std::size_t capacity() const noexcept { return dense_.capacity(); }
    std::size_t size() const noexcept { return dense_.size(); }
    std::size_t max_size() const noexcept { return dense_.max_size(); }

    void clear() noexcept { dense_.clear(); }

    auto begin() noexcept { return dense_.begin(); }
    auto end() noexcept { return dense_.end(); }
    auto begin() const noexcept { return dense_.begin(); }
    auto end() const noexcept { return dense_.end(); }

public:
    T& operator[](std::size_t index) noexcept {
        assert(index < dense_.size());
        return dense_[index];
    }

    const T& operator[](std::size_t index) const noexcept {
        assert(index < dense_.size());
        return dense_[index];
    }

private:
    static constexpr std::size_t DEFAULT_CAPACITY = 262143;
    std::vector<T> dense_;
    std::vector<std::size_t> dense_entities_; 
    //en gros, faut mapper un component = quel entité pour O(1)
};

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

template<typename T>
class SparseSet {
public:
    template<typename U>
    void insert(std::size_t id, U&& value) noexcept {
        dense_.push_back(std::forward<U>(value));
        sparse_[id] = dense_.size() - 1;

        //refaire otute la logique avec le noueau dense_entities_ de dense;
    }

    template<typename U>
    auto emplace_default(U id) noexcept ->
        std::enable_if_t<
            is_index_type_v<U>,
        void> {
        dense_.emplace_back();
        sparse_[id] = dense_.size() - 1;
    }

    template<typename... Ts>
    auto emplace(Ts... ids) noexcept ->
        std::enable_if_t<
            (sizeof...(ids) > 0) &&
            (is_index_type_v<Ts> && ...),
        void> { ((emplace_default<Ts>(ids)), ...); }


    //faire version avec variadic

private:
    std::vector<std::size_t> sparse_;
    DenseArray<T> dense_;

    //pas optimisé, je pourrais faire
    //std::vector<std::pair<std::size_t, T>> dense_;




    //[entité id][component id] -> std::vector<std::size_t>

    //on ajoute un component a un entité
    
    //on regarde si il est deja présent 
    //en prenant l'element a l'id entité du sparse_
    //et en le mettant dans le dense_, si null, on ajoute

    //ensuite, quand on veut retirer un élément
    //on apelle remove_swap()

    //ensuite, on assigne le index du component
    //a l'entité retiré a l'entité qui pointait 
    //sur la case .size() - 1 (dernière).
};

template<typename T>
struct Sparse3 {
private:
    static constexpr std::size_t DEFAULT_DENSE_CAPACITY = 2048;
    static constexpr std::size_t DEFAULT_SPARSE_CAPACITY = 16384;

    // EN GROS, LE SPARSE NE DEVRAIS JAMAIS FAIRE CRASH LE PROGRAMME
    // FAIRE UTILISE STD::OPTIONAL ET RETOURNER SI LE ID EST TROP GRAND
    // NE JAMAIS RESIZE LE TABLEAU SI LE ID EST TROP GRAND, RETIRER LA 
    // LIGNE QUI FAIT CA 
    // if (entity_id >= sparse_.size()) sparse_.resize(entity_id + 1, SIZE_MAX);

    inline constexpr void check_valid_entity_id(std::size_t entity_id) const {
        if (!is_valid_entity_id(entity_id)) 
            throw std::out_of_range(
                std::string("Entity ID ") + std::to_string(entity_id) +
                " out of bounds (max " + std::to_string(sparse_.size() - 1) + ")"
            );
    }
    inline constexpr bool is_valid_entity_id(std::size_t entity_id) const {
        return entity_id < sparse_.size();
    }

public:
    Sparse3(std::size_t init_dense_capacity = DEFAULT_DENSE_CAPACITY,
           std::size_t init_sparse_capacity = DEFAULT_SPARSE_CAPACITY) {
        dense_.reserve(init_dense_capacity);
        binding_.reserve(init_dense_capacity);
        sparse_.reserve(init_sparse_capacity);
        sparse_.resize(init_sparse_capacity, SIZE_MAX);
    }

    Sparse3(const Sparse3&) = default;
    Sparse3& operator=(const Sparse3&) = default;
    Sparse3(Sparse3&&) noexcept = default;
    Sparse3& operator=(Sparse3&&) noexcept = default;
    ~Sparse3() = default;

public:
    template<typename U>
    void insert(std::size_t entity_id, U&& component) noexcept {
        dense_.push_back(std::forward<U>(component));
        std::size_t component_index = dense_.size() - 1;

        if (entity_id >= sparse_.size())
            sparse_.resize(entity_id + 1, SIZE_MAX);

        sparse_[entity_id] = component_index;
        binding_.push_back(entity_id);
    }

    void emplace_default(std::size_t entity_id) noexcept {
        dense_.emplace_back();
        std::size_t component_index = dense_.size() - 1;

        if (entity_id >= sparse_.size())
            sparse_.resize(entity_id + 1, SIZE_MAX);

        sparse_[entity_id] = component_index;
        binding_.push_back(entity_id);
    }

    void remove_swap(std::size_t entity_id) {
        check_valid_entity_id(entity_id);

        std::size_t component_index = sparse_[entity_id];
        std::size_t last_component_index = dense_.size() - 1;
        std::size_t last_entity_id = binding_[last_component_index];

        std::swap(dense_[component_index], dense_[dense_.size() - 1]);
        dense_.pop_back();

        std::swap(binding_[component_index], binding_[last_component_index]);
        binding_.pop_back();

        sparse_[last_entity_id] = component_index;
        sparse_[entity_id] = SIZE_MAX;
    }

public:
    bool contains(std::size_t entity_id) const {
        return is_valid_entity_id(entity_id) && sparse_[entity_id] != SIZE_MAX;
    }

    std::size_t count() const noexcept { return dense_.size(); }
    std::size_t capacity() const noexcept { return sparse_.capacity(); }
    std::size_t is_empty() const noexcept { return sparse_.empty(); }

    void clear_sparse() noexcept { sparse_.clear(); }
    void clear_dense() noexcept { dense_.clear(); }
    void clear_binding() noexcept { binding_.clear(); }

    void reserve(std::size_t new_capacity) noexcept { sparse_.reserve(new_capacity); }

    void shrink_sparse_to_fit() noexcept { sparse_.shrink_to_fit(); }
    void shrink_dense_to_fit() noexcept { dense_.shrink_to_fit(); }
    void shrink_binding_to_fit() noexcept { binding_.shrink_to_fit(); }

public:
    auto begin() noexcept { return dense_.begin(); }
    auto end() noexcept { return dense_.end(); }
    auto begin() const noexcept { return dense_.begin(); }
    auto end() const noexcept { return dense_.end(); }

public:
    T& operator[](std::size_t entity_id) {
        check_valid_entity_id(entity_id);
        return dense_[sparse_[entity_id]];
    }

    const T& operator[](std::size_t entity_id) const {
        check_valid_entity_id(entity_id);
        return dense_[sparse_[entity_id]];
    }

    T& get(std::size_t entity_id) {
        check_valid_entity_id(entity_id);
        return dense_[sparse_[entity_id]];
    }

private:
    std::vector<std::size_t> sparse_; // contient un entity_id -> component_id
    std::vector<std::size_t> binding_; // contient un composante_id -> entity_id
    std::vector<T> dense_;  //contient un component_id -> component (T)
};

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

    inline constexpr bool is_valid_entity_id(std::size_t entity_id) const {
        return entity_id < sparse_.size();
    }

public:
    Sparse(std::size_t init_dense_capacity = DEFAULT_DENSE_CAPACITY,
        std::size_t init_sparse_capacity = DEFAULT_SPARSE_CAPACITY) {
        dense_.reserve(init_dense_capacity);
        binding_.reserve(init_dense_capacity);
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

        dense_.push_back(std::forward<U>(component));
        std::size_t component_index = dense_.size() - 1;

        sparse_[entity_id] = component_index;
        binding_.push_back(entity_id);
    }

    void emplace_default(std::size_t entity_id) noexcept {
        if (!is_valid_entity_id(entity_id)) {
            error_not_enough_capacity(
                "Sparse vector too small",
                entity_id + 1,
                sparse_.size()
            ); return;
        }

        dense_.emplace_back();
        std::size_t component_index = dense_.size() - 1;

        sparse_[entity_id] = component_index;
        binding_.push_back(entity_id);
    }

    void remove_swap(std::size_t entity_id) {
        if (!is_valid_entity_id(entity_id)) {
            error_not_enough_capacity(
                "Sparse vector too small",
                entity_id + 1,
                sparse_.size()
            ); return;
        }

        std::size_t component_index = sparse_[entity_id];
        std::size_t last_component_index = dense_.size() - 1;
        std::size_t last_entity_id = binding_[last_component_index];

        std::swap(dense_[component_index], dense_[dense_.size() - 1]);
        dense_.pop_back();

        std::swap(binding_[component_index], binding_[last_component_index]);
        binding_.pop_back();

        sparse_[last_entity_id] = component_index;
        sparse_[entity_id] = SIZE_MAX;
    }

public:
    bool contains(std::size_t entity_id) const {
        return is_valid_entity_id(entity_id) && sparse_[entity_id] != SIZE_MAX;
    }

    std::size_t count() const noexcept { return dense_.size(); }
    std::size_t capacity() const noexcept { return sparse_.capacity(); }
    std::size_t is_empty() const noexcept { return sparse_.empty(); }

    void clear_sparse() noexcept { sparse_.clear(); }
    void clear_dense() noexcept { dense_.clear(); }
    void clear_binding() noexcept { binding_.clear(); }

    void reserve(std::size_t new_capacity) noexcept { sparse_.reserve(new_capacity); }

    void shrink_sparse_to_fit() noexcept { sparse_.shrink_to_fit(); }
    void shrink_dense_to_fit() noexcept { dense_.shrink_to_fit(); }
    void shrink_binding_to_fit() noexcept { binding_.shrink_to_fit(); }

public:
    auto begin() noexcept { return dense_.begin(); }
    auto end() noexcept { return dense_.end(); }
    auto begin() const noexcept { return dense_.begin(); }
    auto end() const noexcept { return dense_.end(); }

public:
    std::optional<T&> operator[](std::size_t entity_id) noexcept {
        if (!is_valid_entity_id(entity_id)) {
            error_not_enough_capacity(
                "Sparse vector too small",
                entity_id + 1,
                sparse_.size()
            ); return std::nullopt;
        }
        return dense_[sparse_[entity_id]];
    }

    const std::optional<T&> operator[](std::size_t entity_id) const noexcept {
        if (!is_valid_entity_id(entity_id)) {
            error_not_enough_capacity(
                "Sparse vector too small",
                entity_id + 1,
                sparse_.size()
            ); return std::nullopt;
        }
        return dense_[sparse_[entity_id]];
    }

    std::optional<T&> get(std::size_t entity_id) noexcept & {
        if (!is_valid_entity_id(entity_id)) {
            error_not_enough_capacity(
                "Sparse vector too small",
                entity_id + 1,
                sparse_.size()
            ); return std::nullopt;
        }
        return dense_[sparse_[entity_id]];
    }

    std::optional<const T&> get(std::size_t entity_id) const noexcept & {
        if (!is_valid_entity_id(entity_id)) {
            error_not_enough_capacity(
                "Sparse vector too small",
                entity_id + 1,
                sparse_.size()
            ); return std::nullopt;
        }
        return dense_[sparse_[entity_id]];
    }

private:
    std::vector<std::size_t> sparse_; // contient un entity_id -> component_id
    std::vector<std::size_t> binding_; // contient un composante_id -> entity_id
    std::vector<T> dense_;  //contient un component_id -> component (T)
};


class Object {
public:
    void test() const noexcept {
        std::cout << "Test :)" << std::endl;
    }
};


int main() {
    std::cout << "Hello World!\n";

    DenseArray<Object> dense;

    dense.emplace_back();
    dense.emplace_back();

    dense.emplace_back();
    dense.emplace_back();

    std::cout << dense.size() << std::endl;

    dense.swap_and_pop(0);

    std::cout << dense.size() << std::endl;
    dense.swap_and_pop(0);

    //dense.push_back(6);
    //dense.push_back(7);
    //dense.push_back(8);
    //dense.push_back(5);
    //dense.push_back(1);
    //dense.push_back(9);

    //dense.print_all();

    //dense.swap_and_pop(3);

    //dense.print_all();

    //dense.swap_and_pop(2);

    //dense.print_all();
    

    dense[0].test(); //imagine faire un variadic qui execute
    //la ligne pour tous les elements du dense
}
