package com.vz.backend.business.service;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import com.vz.backend.business.domain.Tag;
import com.vz.backend.business.repository.ITagRepository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;

@Service
public class TagService extends BaseService<Tag> {

	@Autowired
	private ITagRepository tagRepository;

	@Override
	public IRepository<Tag> getRepository() {
		return tagRepository;
	}

	public Tag addTag(Tag tag) {
		tag.valid();
		Tag exist = tagRepository.findByClientIdAndCreateByAndNameAndActiveTrue(BussinessCommon.getClientId(),
				BussinessCommon.getUserId(), tag.getName());

		if (exist != null)
			throw new RestExceptionHandler(Message.TAG_EXIST);

		return tagRepository.save(tag);
	}

	public Tag update(Long id, String name) {
		Tag exist = valid(id, Message.NOT_FOUND_TAG);
		exist.setName(name);
		return tagRepository.save(exist);
	}

	public boolean delete(Long id) {
		Tag exist = valid(id, Message.NOT_FOUND_TAG);
		exist.setActive(false);
		tagRepository.save(exist);
		return true;
	}

	public Page<Tag> getListTagByCreator(boolean detailPageSize, int page) {
		Pageable pageable;
		if (detailPageSize == true) {
			pageable = BussinessCommon.castToPageable(page, 5);
		} else {
			pageable = BussinessCommon.castToPageable(page);
		}

		return tagRepository.findByClientIdAndCreateByAndActiveTrueOrderByNameAscCreateDateDesc(
				BussinessCommon.getClientId(), BussinessCommon.getUserId(), pageable);
	}

	public Page<Tag> search(String text, int page) {
		return tagRepository.searchTagByName(BussinessCommon.getClientId(), BussinessCommon.getUserId(),
				BussinessCommon.convert(text), BussinessCommon.castToPageable(page));
	}
	
	public List<Tag> searchWithoutPage(String text) {
		return tagRepository.searchTagByName(BussinessCommon.getClientId(), BussinessCommon.getUserId(),
				BussinessCommon.convert(text));
	}
	
	public List<Tag> getListTagByCreatorWithoutPage() {
		return tagRepository.findByClientIdAndCreateByAndActiveTrueOrderByNameAscCreateDateDesc(BussinessCommon.getClientId(),
				BussinessCommon.getUserId());
	}
	
	public List<Tag> getListTagByObjId(Long objId, DocumentTypeEnum type) {
		return tagRepository.findByClientIdAndObjIdAndTypeAndActiveTrue(BussinessCommon.getClientId(), objId, type, BussinessCommon.getUserId());

	}

}
