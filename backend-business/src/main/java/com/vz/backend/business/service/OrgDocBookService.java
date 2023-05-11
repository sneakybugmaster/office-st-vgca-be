package com.vz.backend.business.service;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.vz.backend.business.domain.OrgDocBook;
import com.vz.backend.business.repository.IOrgDocBookRepository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;
import org.springframework.transaction.annotation.Transactional;

@Service
public class OrgDocBookService extends BaseService<OrgDocBook> {

	@Autowired
	IOrgDocBookRepository orgDocBookRepo;

	@Override
	public IRepository<OrgDocBook> getRepository() {
		return orgDocBookRepo;
	}

	@Transactional
	public void add(Long bookId, Set<Long> listOrgIds) {
		Set<Long> listOrgOfBook = orgDocBookRepo.findOrgIdByBookIdAndClientIdAndActive(bookId, BussinessCommon.getClientId(), true);
		Set<Long> deleteIds = new HashSet<>(listOrgOfBook);
		deleteIds.removeAll(listOrgIds);
		Set<Long> addIds = new HashSet<>(listOrgIds);
		addIds.removeAll(listOrgOfBook);
		if (!deleteIds.isEmpty()) orgDocBookRepo.deleteByBookIdAndOrgIdIn(bookId, deleteIds);
		if (!addIds.isEmpty()) {
			List<OrgDocBook> listAdd = new ArrayList<>();
			for (Long id : addIds) {
				listAdd.add(new OrgDocBook(bookId, id));
			}
			orgDocBookRepo.saveAll(listAdd);
		}
	}
	
	public void add(Long bookId, List<Long> listOrgIds) {
		List<OrgDocBook> listData = new ArrayList<>();
		if (listOrgIds != null && !listOrgIds.isEmpty()) {
			OrgDocBook item;
			List<OrgDocBook> orgOfBookId = orgDocBookRepo.findByBookIdAndClientId(bookId, BussinessCommon.getClientId());
			if (orgOfBookId != null && !orgOfBookId.isEmpty()) {
				for (OrgDocBook element : orgOfBookId) {
					element.setActive(false);
				}
			}
			for (Long orgId : listOrgIds) {
				if (orgOfBookId != null && !orgOfBookId.isEmpty()) {
					for (int j = 0; j < orgOfBookId.size(); j++) {
						if (orgOfBookId.get(j).getOrgId().equals(orgId)) {
							orgOfBookId.get(j).setActive(true);
							break;
						}
						// last item
						if (j == orgOfBookId.size() - 1) {
							item = new OrgDocBook();
							item.setBookId(bookId);
							item.setActive(true);
							item.setOrgId(orgId);
							listData.add(item);
						}
					}
				} else {
					item = new OrgDocBook();
					item.setBookId(bookId);
					item.setActive(true);
					item.setOrgId(orgId);
					listData.add(item);
				}
			}
			orgDocBookRepo.saveAll(listData);
		}
	}

	public List<Long> findOrgIdByBookIdAndActive(Long bookId, Boolean active) {
		return orgDocBookRepo.findOrgIdByBookIdAndActive(bookId, active);
	}
}
