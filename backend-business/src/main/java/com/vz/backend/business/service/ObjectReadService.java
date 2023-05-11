package com.vz.backend.business.service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.vz.backend.business.domain.ObjectRead;
import com.vz.backend.business.repository.IObjectReadRepository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;

@Service
public class ObjectReadService extends BaseService<ObjectRead> {

	@Autowired
	IObjectReadRepository objReadRepository;
	
	@Override
	public IRepository<ObjectRead> getRepository() {
		return objReadRepository;
	}

	public ObjectRead find(Long userId, Long objId, DocumentTypeEnum type) {
		return objReadRepository.findFirstByClientIdAndUserIdAndObjIdAndTypeAndActiveTrue(BussinessCommon.getClientId(), userId, objId, type);
	}
	
	public void setRead(Long userId, Long objId, DocumentTypeEnum type, boolean read) {
		ObjectRead obj = find(userId, objId, type);
		if (obj == null) {
			objReadRepository.save(new ObjectRead(userId, type, objId, read));
			return;
		}

		if (read != obj.isRead()) {
			obj.setRead(read);
			objReadRepository.save(obj);
		}
	}
	
	public List<ObjectRead> find(Long userId, List<Long> objIds, DocumentTypeEnum type) {
		return objReadRepository.findByClientIdAndUserIdAndObjIdInAndTypeAndActiveTrue(BussinessCommon.getClientId(), userId, objIds, type);
	}
	
	public long countByRead(Long userId, List<Long> objIds, DocumentTypeEnum type) {
		return objReadRepository.countByClientIdAndUserIdAndObjIdInAndTypeAndReadTrueAndActiveTrue(BussinessCommon.getClientId(), userId, objIds, type);
	}
	
	public void setRead(Long userId, List<Long> objIds, DocumentTypeEnum type, boolean read) {
		List<ObjectRead> rs = new ArrayList<>();
		for (Long objId : objIds) {
			ObjectRead obj = find(userId, objId, type);
			if (obj == null) {
				rs.add(new ObjectRead(userId, type, objId, read));
			}

			if (obj != null && read != obj.isRead()) {
				obj.setRead(read);
				rs.add(obj);
			}
		}
		objReadRepository.saveAll(rs);
	}
	
	public Map<Long, Boolean> getObjReadMap(Long userId, List<Long> objIds, DocumentTypeEnum type) {
		List<ObjectRead> objReads = find(userId, objIds, type);
		Map<Long, Boolean> objReadMap = new HashMap<>();
		objReads.forEach(i -> {
			Long key = i.getObjId();
			if (!objReadMap.containsKey(key)) {
				objReadMap.put(key, i.isRead());
			}
		});

		return objReadMap;
	}
	
	public boolean setRead(Long key, Map<Long, Boolean> objReadMap) {
		if (objReadMap.containsKey(key)) {
			return objReadMap.get(key) == null ? Boolean.FALSE : objReadMap.get(key);
		}
		return false;
	}
}
