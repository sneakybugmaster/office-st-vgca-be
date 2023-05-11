package com.vz.backend.business.service;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.vz.backend.business.domain.DocumentInOut;
import com.vz.backend.business.repository.IDocumentInOutRepository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;

@Service
public class DocumentInOutService extends BaseService<DocumentInOut> {

	@Autowired
	IDocumentInOutRepository docInOutRepo;

	@Override
	public IRepository<DocumentInOut> getRepository() {
		return docInOutRepo;
	}

	public void add(Long docOutId, Set<Long> docInIds) {
		Set<Long> listDocInRelate = docInOutRepo.findDocInIdByDocOutIdAndClientId(docOutId, BussinessCommon.getClientId());
		Set<Long> deleteIds = new HashSet<>(listDocInRelate);
		deleteIds.removeAll(docInIds);
		Set<Long> addIds = new HashSet<>(docInIds);
		addIds.removeAll(listDocInRelate);
		if (!deleteIds.isEmpty()) docInOutRepo.deleteByDocOutIdAndDocInIdIn(docOutId, deleteIds);
		if (!addIds.isEmpty()) {
			List<DocumentInOut> listAdd = new ArrayList<>();
			for (Long id : addIds) {
				listAdd.add(new DocumentInOut(id, docOutId));
			}
			docInOutRepo.saveAll(listAdd);
		}
	}
}
