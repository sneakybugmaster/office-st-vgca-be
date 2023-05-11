package com.vz.backend.business.service;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.vz.backend.business.domain.CategoryDocBook;
import com.vz.backend.business.repository.ICategoryDocBookRepository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;

@Service
public class CategoryDocBookService extends BaseService<CategoryDocBook>{
	@Autowired
	ICategoryDocBookRepository categoryDocBookRepo;

	@Override
	public IRepository<CategoryDocBook> getRepository() {
		return categoryDocBookRepo;
	}
	
	public void add(Long bookId, List<Long> listOrgIds) {
		List<CategoryDocBook> listData = new ArrayList<>();
		if (listOrgIds != null && !listOrgIds.isEmpty()) {
			CategoryDocBook item;
			List<CategoryDocBook> orgOfBookId = categoryDocBookRepo.findByBookIdAndClientId(bookId, BussinessCommon.getClientId());
			if (orgOfBookId != null && !orgOfBookId.isEmpty()) {
				for (CategoryDocBook element : orgOfBookId) {
					element.setActive(false);
				}
			}
			for (Long orgId : listOrgIds) {
				if (orgOfBookId != null && !orgOfBookId.isEmpty()) {
					for (int j = 0; j < orgOfBookId.size(); j++) {
						if (orgOfBookId.get(j).getCategoryId().equals(orgId)) {
							orgOfBookId.get(j).setActive(true);
							break;
						}
						// last item
						if (j == orgOfBookId.size() - 1) {
							item = new CategoryDocBook();
							item.setBookId(bookId);
							item.setActive(true);
							item.setCategoryId(orgId);
							listData.add(item);
						}
					}
				} else {
					item = new CategoryDocBook();
					item.setBookId(bookId);
					item.setActive(true);
					item.setCategoryId(orgId);
					listData.add(item);
				}
			}
			categoryDocBookRepo.saveAll(listData);
		}
	}

	public List<Long> findCategoryIdByBookIdAndActive(Long bookId, Boolean active) {
		return categoryDocBookRepo.findCategoryIdByBookIdAndActive(bookId, BussinessCommon.getClientId(), active);
	}
}
