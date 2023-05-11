package com.vz.backend.core.service;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.domain.Secretary;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.repository.ISecretaryRepository;
import org.springframework.transaction.annotation.Transactional;

@Service
public class SecretaryService extends BaseService<Secretary> {

	@Autowired
	private ISecretaryRepository secretaryRepository;

	@Override
	public IRepository<Secretary> getRepository() {
		return secretaryRepository;
	}

	/**
	 * Phân công thư kí - sếp
	 * 
	 * @param bossId
	 * @param newsList
	 */
	@Transactional
	public void save(Long bossId, List<Secretary> newsList) {
		if (bossId == null)
			return;
		newsList = newsList != null ? newsList : new ArrayList<>();
		List<Long> nUserIds = newsList.stream().map(Secretary::getUserId).collect(Collectors.toList());
		List<Long> oUserIds = findSecretaryIdByBossId(bossId, true);
		List<Long> all = new ArrayList<>();
		all.addAll(nUserIds);
		all.addAll(oUserIds);

		List<Long> addList = new ArrayList<>();
		List<Long> delList = new ArrayList<>();
		all.forEach(a -> {
			if (!oUserIds.contains(a) && nUserIds.contains(a)) {
				// add
				addList.add(a);
			} else if (oUserIds.contains(a) && !nUserIds.contains(a)) {
				// del
				delList.add(a);
			}
		});

		try {
			List<Secretary> s = Secretary.getInstance().add(bossId, addList);
			secretaryRepository.saveAll(s);
			secretaryRepository.deleteByBossIdAndUserIdIn(bossId, delList);
		} catch (Exception e) {
			e.printStackTrace();
			throw new RestExceptionHandler(Message.SECRETARY_ERROR);
		}
	}

	/**
	 * Tìm danh sách thư kí (Lấy toàn bộ thông tin)
	 * 
	 * @param bossId
	 * @return
	 */
	public List<Secretary> findSecretaryByBossId(Long bossId, Boolean active) {
		return secretaryRepository.findByBossIdAndClientIdAndActive(bossId, BussinessCommon.getClientId(), active);
	}
	
	public List<Secretary> findSecretaryByBossId(Long bossId, Long clientId, Boolean active) {
		return secretaryRepository.findByBossIdAndClientIdAndActive(bossId, clientId, active);
	}

	/**
	 * Tìm danh sách thư kí (Chỉ lấy chỉ mục)
	 * 
	 * @param bossId
	 * @return
	 */
	public List<Long> findSecretaryIdByBossId(Long bossId, Boolean active) {
		List<Secretary> sList = findSecretaryByBossId(bossId, active);
		return sList.stream().map(Secretary::getUserId).collect(Collectors.toList());
	}
}
