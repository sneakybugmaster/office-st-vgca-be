package com.vz.backend.core.service;

import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;

import com.vz.backend.core.domain.RootModel;

public interface IService<T extends RootModel> {

	long count();

	void deleteAll(Iterable<T> entities);

	void delete(T entity);

	void deleteById(Long id);

	boolean existsById(Long id);

	T save(T entity);

	T saveAndFlush(T entity);

	List<T> saveAll(Iterable<T> entities);

	T getOne(Long id);

	Optional<T> findById(Long id);

	List<T> findAll();

	List<T> findAllById(Iterable<Long> ids);

	List<T> findAll(Sort sort);

	Page<T> findAll(Pageable pageable);

	Page<T> findAllByClientId(Long clientId, Pageable pageable);

	List<T> findByClientId(Long clientId, Pageable pageable);

	List<T> findByClientId(Long clientId);

	T findByClientIdAndId(Long clientId, Long id);

	List<T> findByClientIdAndActive(Long clientId, boolean active, Pageable pageable);

	List<T> findByClientIdAndActive(Long clientId, boolean active);

	List<T> findByClientIdAndActive(Long clientId, boolean active, int page);

	List<T> findByClientId(Long clientId, int page);

	List<T> findByClientId(Long clientId, Sort sort);

	List<T> findByClientIdAndActive(Long clientId, Boolean active, Sort sort);
}
