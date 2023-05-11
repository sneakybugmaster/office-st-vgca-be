package com.vz.backend.business.service.docInternal;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.vz.backend.business.config.DocInternalTrackingEnum;
import com.vz.backend.business.domain.documentInternal.DocInternalTracking;
import com.vz.backend.business.dto.document.ApproverDto;
import com.vz.backend.business.repository.docInternal.IDocInternalTrackingRepository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;

@Service
public class DocInternalTrackingService extends BaseService<DocInternalTracking> {
	@Autowired
	private IDocInternalTrackingRepository docInternalTrackingRepo;
	
	@Override
	public IRepository<DocInternalTracking> getRepository() {
		return docInternalTrackingRepo;
	}

	public void add(Long docId, DocInternalTrackingEnum action) {
		docInternalTrackingRepo.save(new DocInternalTracking(docId, BussinessCommon.getUserId(), action));
	}

	public void add(Long docId, DocInternalTrackingEnum action, Long commentId) {
		docInternalTrackingRepo.save(new DocInternalTracking(docId, BussinessCommon.getUserId(), action, commentId));
	}

	public List<ApproverDto> getListReturnComment(Long docId) {
		return docInternalTrackingRepo.findByDocIdAndAction(docId, DocInternalTrackingEnum.TU_CHOI_DUYET, BussinessCommon.getClientId());
	}
}
