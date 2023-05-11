package com.vz.backend.business.service;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.vz.backend.business.domain.DocumentUser;
import com.vz.backend.business.repository.IDocumentUserRepository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;

@Service
public class DocumentUserService extends BaseService<DocumentUser> {

	@Autowired
	IDocumentUserRepository docUserRepo;

	@Override
	public IRepository<DocumentUser> getRepository() {
		return docUserRepo;
	}

	public Boolean setImportant(DocumentTypeEnum docType, Long docId, Boolean important) {
		DocumentUser du = docUserRepo.findByDocTypeAndDocIdAndUserIdAndClientId(docType, docId,
				BussinessCommon.getUserId(), BussinessCommon.getClientId());
		if (du == null) {
			du = new DocumentUser();
			du.setDocType(docType);
			du.setDocId(docId);
			du.setUserId(BussinessCommon.getUserId());
			du.setImportant(important);
			du = docUserRepo.save(du);
			return du.getImportant();
		} else {
			du.setImportant(important);
			du = docUserRepo.save(du);
			return du.getImportant();
		}
	}

	public List<DocumentUser> findByDocTypeAndDocIdInAndUserId(DocumentTypeEnum docType, List<Long> idList,
			Long userId) {
		return docUserRepo.findByDocTypeAndDocIdInAndUserIdAndClientId(docType, idList, userId,
				BussinessCommon.getClientId());
	}

}
