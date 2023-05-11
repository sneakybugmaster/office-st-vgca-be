package com.vz.backend.business.service;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import com.vz.backend.business.domain.DocumentInManipulation;
import com.vz.backend.business.repository.IDocumentInManipulationRepository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Constant;
import com.vz.backend.core.config.DocumentInHandleStatusEnum;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.dto.LabelValueDto;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;

@Service
public class DocumentInManipulationService extends BaseService<DocumentInManipulation>{

	@Autowired
	IDocumentInManipulationRepository manipulationRepo;
	
	@Autowired
	private ObjectReadService objReadService;
	
	@Override
	public IRepository<DocumentInManipulation> getRepository() {
		return manipulationRepo;
	}
	
	public void save(Long docId, List<Long> toUsers, DocumentInHandleStatusEnum status) {
		User user = BussinessCommon.getUser();
		if (BussinessCommon.isEmptyList(toUsers)) return;
		List<DocumentInManipulation> rsList = new ArrayList<>();

		//valid
		List<Long> oToUsers = manipulationRepo.findByDocIdAndFrUserAndToUsers(docId, user.getId(), toUsers, DocumentInHandleStatusEnum.CHO_CHO_Y_KIEN, user.getClientId(), true);
		toUsers.forEach(i -> {
			Optional<Long> toUser = oToUsers.stream().filter(j->j.equals(i)).findFirst();
			if(!toUser.isPresent()) {
				rsList.add(new DocumentInManipulation(docId, i, status));
			}
		});
		
		manipulationRepo.saveAll(rsList);
	}
	
	/**
	 * return list from user
	 * @param docId
	 * @param userId
	 * @param status
	 * @return
	 */
	public List<Long> update(Long docId, Long userId, DocumentInHandleStatusEnum status) {
		List<DocumentInManipulation> olds = findByDocIdAndUserId(docId, userId);
		if(!olds.isEmpty()) {
			olds.forEach(i-> i.setHandleStatus(status));
			manipulationRepo.saveAll(olds);
		}
		
		return olds.stream().map(DocumentInManipulation::getFrUser).distinct().collect(Collectors.toList());
	}
	
	public List<DocumentInManipulation> findByDocIdAndUserId(Long docId, Long userId) {
		return manipulationRepo.findByDocIdAndToUserAndHandleStatusAndClientIdAndActive(docId, userId, DocumentInHandleStatusEnum.CHO_CHO_Y_KIEN, BussinessCommon.getClientId(), true);
	}
	
	public boolean hasAskToUser(Long docId, Long userId) {
		return manipulationRepo.hasAskToUser(docId, userId, BussinessCommon.getClientId());
	}
	
	public Map<Long, Boolean> hasAskToUser(List<Long> docIds, Long userId) {
		List<LabelValueDto<Long>> kvs = manipulationRepo.hasAskToUser(docIds, userId, BussinessCommon.getClientId());
		HashMap<Long, Boolean> rs = new HashMap<>();
		for (LabelValueDto<Long> i : kvs) {
			rs.put(i.getLabel(), i.getValue() > 0);
		}
		return rs;
	}
	
	public boolean hasRelatedIdea(Long docId, Long userId) {
		return manipulationRepo.hasRelatedIdea(docId, userId, BussinessCommon.getClientId());
	}
	
	public Page<DocumentInManipulation> findByUserId(String text, String type, Pageable pageable) {
		Long toUser = null;
		Long frUser = null;
		Long userId = BussinessCommon.getUserId();
		
		if(Constant.TYPE_CHO_Y_KIEN.equals(type)) toUser = userId;
		if(Constant.TYPE_XIN_Y_KIEN.equals(type)) frUser = userId;
		Page<DocumentInManipulation> rs = manipulationRepo.findByUserId(text, toUser, frUser, BussinessCommon.getClientId(), true, pageable);
		setRead(rs);
		return rs;
	}
	
	private void setRead(Page<DocumentInManipulation> rs) {
		if (BussinessCommon.isEmptyPage(rs))
			return;

		List<DocumentInManipulation> list = rs.getContent();
		List<Long> docIds = list.stream().map(DocumentInManipulation::getDocId).collect(Collectors.toList());
		Map<Long, Boolean> objReadMap = objReadService.getObjReadMap(BussinessCommon.getUserId(), docIds,
				DocumentTypeEnum.VAN_BAN_DEN);

		for (DocumentInManipulation i : list) {
			i.setRead(objReadService.setRead(i.getDocId(), objReadMap));
		}
	}
	
	public boolean getHandleStatusByDocId(Long userId, Long docId) {
		return manipulationRepo.getHandleStatusByDocId(userId, docId, BussinessCommon.getClientId());
	}

	public List<Long> findFromUserByToUserAndDocId(Long userId, Long docId) {
		return manipulationRepo.findFrUserByToUserAndDocIdAndClientIdAndActive(userId, docId, BussinessCommon.getClientId(), true);
	}

	public List<Long> findToUserByFromUserAndDocId(Long userId, Long docId) {
		return manipulationRepo.findToUserByFrUserAndDocIdAndClientIdAndActive(userId, docId, BussinessCommon.getClientId(), true);
	}
	
	public long waitComment(Long userId, Date startDate, Date endDate) {
		return manipulationRepo.waitComment(userId, startDate, endDate, BussinessCommon.getClientId());
	}
	
	public List<Long> findRelatedByDocId(List<Long> docIds) {
		return manipulationRepo.findRelatedByDocId(docIds, BussinessCommon.getClientId());
	}
}
