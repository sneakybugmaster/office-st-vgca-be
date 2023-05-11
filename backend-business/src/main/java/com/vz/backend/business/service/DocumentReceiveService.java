package com.vz.backend.business.service;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import com.vz.backend.core.config.HandleTypeEnum;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.vz.backend.business.domain.DocumentReceive;
import com.vz.backend.business.dto.DocumentReceiveDto;
import com.vz.backend.business.dto.document.DocumentReceiveBasicDto;
import com.vz.backend.business.repository.IDocumentReceiveRepository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Constant;
import com.vz.backend.core.config.DocumentStatusEnum;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;
import com.vz.backend.core.service.UserService;

@Service
public class DocumentReceiveService extends BaseService<DocumentReceive> {

	@Autowired
	IDocumentReceiveRepository docReceiveRepo;

	@Autowired
	UserService uService;

	@Override
	public IRepository<DocumentReceive> getRepository() {
		return docReceiveRepo;
	}

//	public DocumentReceive add(Long docId, Long userId, Long orgId) {
//		return docReceiveRepo.save(new DocumentReceive(docId, userId, orgId));
//	}

	public List<DocumentReceive> findByClientIdAndDocId(Long clientId, Long docId) {
		return docReceiveRepo.findByClientIdAndDocId(clientId, docId);
	}

	public List<DocumentReceive> findByClientIdAndDocId(Long clientId, Long[] docId) {
		return docReceiveRepo.findByClientIdAndDocId(clientId, docId);
	}

	public List<DocumentReceive> findByClientIdAndUserId(Long clientId, Long userId) {
		return docReceiveRepo.findByClientIdAndUserId(clientId, userId, Constant.RECEIVER_TYPE_USER);
	}

	public void updateListReceivePerson(Long docId, List<DocumentReceive> listReceiveUpdate) {
		List<DocumentReceive> listReceiveBefore = findByClientIdAndDocId(BussinessCommon.getClientId(), docId);
		for (DocumentReceive element : listReceiveBefore) {
			boolean flag = true;
			for (DocumentReceive d : listReceiveUpdate) {
				if (!d.getUserId().equals(element.getUserId())) {
					flag = false;
					docReceiveRepo.save(d);
				}
			}
			if (flag) {
				docReceiveRepo.deleteById(element.getId());
			}
		}
	}

	public void removeListRd(Long docId, List<Long> idList) {
		docReceiveRepo.updateByIdList(docId, idList, BussinessCommon.getClientId());
	}

	public void deleteByDocId(Long docId, Long clientId) {
		docReceiveRepo.deleteByDocId(docId, clientId);
	}

	public List<DocumentReceiveDto> getFullNameByDocId(Long[] docId) {
		List<DocumentReceive> drList = findByClientIdAndDocId(BussinessCommon.getClientId(), docId);
		if (drList == null) {
			return null;
		}
		List<DocumentReceiveDto> rsList = new ArrayList<>();
		List<Long> idList = drList.stream().map(DocumentReceive::getUserId).collect(Collectors.toList());
		if (idList == null) {
			return null;
		}
		Long[] ids = idList.stream().toArray(Long[]::new);
		if (ids == null) {
			return null;
		}
		List<User> uList = uService.findByListUserNameAndClientId(ids, BussinessCommon.getClientId());
		uList.forEach(u -> {
			Optional<DocumentReceive> dr = drList.stream().filter(i -> i.getUserId().equals(u.getId())).findFirst();
			if (dr.isPresent()) {
				DocumentReceiveDto dto = new DocumentReceiveDto();
				dto.setUserId(dr.get().getUserId());
				dto.setUsername(u.getFullName());
				dto.setFullname(u.getFullName());
				dto.setDocId(dr.get().getDocId());
				rsList.add(dto);
			}
		});

		return rsList;
	}

	public List<String> getFullNameByDocId(Long docId, List<DocumentReceiveDto> dtoList) {
		List<String> sList = new ArrayList<>();
		dtoList.forEach(i -> {
			if (docId.equals(i.getDocId())) {
				sList.add(i.getFullname());
			}
		});
		return sList;
	}

	public DocumentReceive findByClientIdAndDocIdAndUserId(Long clientId, Long docId, Long toUserId) {
		return docReceiveRepo.findByClientIdAndDocIdAndUserId(clientId, docId, toUserId, Constant.RECEIVER_TYPE_USER);
	}

	@Override
	public DocumentReceive save(DocumentReceive dc) {
		return docReceiveRepo.save(dc);
	}

	public List<DocumentReceive> findByClientIdAndDocIdAndType(Long clientId, Long docId, String type) {
		return docReceiveRepo.findByClientIdAndDocIdAndType(clientId, docId, type);
	}

	public List<DocumentReceive> findByDocIdAndTypeAndClientIdAndActive(Long docId, String type, Long clientId, Boolean active ) {
		return docReceiveRepo.findByDocIdAndTypeAndClientIdAndActive(docId, type, clientId, active);
	}
	
	public List<DocumentReceive> findByClientIdAndDocIdTypeNotTransfer(Long clientId, Long docId) {
		return docReceiveRepo.findByClientIdAndDocIdAndTypeNotTransfer(clientId, docId);
	}

	public List<DocumentReceive> findDRByClientIdAndDocIdTypeNotTransfer(Long clientId, Long docId, HandleTypeEnum handleType) {
		return docReceiveRepo.findDocumentReceiveByDocIdAndTypeNotTransfer(clientId, docId, handleType);
	}
	
	public List<DocumentReceiveBasicDto> findDocumentReceiveDtoByClientIdAndDocIdAndType(Long clientId, Long docId, String type) {
		return docReceiveRepo.findDocumentReceiveDtoByClientIdAndDocIdAndType(clientId, docId, type);
	}

	public Long countDocByUser() {
		User u = BussinessCommon.getUser();
		return docReceiveRepo.countDocByUser(u.getClientId(), u.getId(), u.getOrg(), u.isLead());
	}
	
	public void setStatus(Long docId, Long userId, DocumentStatusEnum status) {
		List<DocumentReceive> rs = docReceiveRepo.findByClientIdAndDocIdAndReceiveIdAndActiveTrue(BussinessCommon.getClientId(), docId, userId);
		DocumentReceive dr = rs.isEmpty() ? null : rs.get(0); 
		if (dr == null) {
			docReceiveRepo.save(new DocumentReceive(docId, userId, status));
			return;
		}

		if (dr.getStatus() == null || !status.equals(dr.getStatus())) {
			dr.setStatus(status);
			docReceiveRepo.save(dr);
		}
	}

}
