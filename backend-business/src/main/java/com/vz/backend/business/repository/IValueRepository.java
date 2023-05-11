package com.vz.backend.business.repository;

import java.util.List;

import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.Values;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IValueRepository extends IRepository<Values> {
	List<Values> findByClientIdAndCatIdAndFormId(Long client, long catId, long formId);

	List<Values> findByClientIdAndCatIdAndFieldsId(Long client, long catId, long fieldsId);

	Values findByClientIdAndCatIdAndFormIdAndFieldsId(Long client, long catId, long formId, long fieldsId);
}
