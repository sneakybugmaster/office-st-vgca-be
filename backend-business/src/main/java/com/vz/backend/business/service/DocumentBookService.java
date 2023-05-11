package com.vz.backend.business.service;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.Optional;

import javax.persistence.EntityManagerFactory;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import com.vz.backend.business.domain.DocumentBook;
import com.vz.backend.business.dto.DocumentBookDetailDto;
import com.vz.backend.business.dto.DocumentBookDto;
import com.vz.backend.business.dto.DocumentBookInitDto;
import com.vz.backend.business.dto.DocumentBookWithListOrgIds;
import com.vz.backend.business.repository.IDocumentBookRepository;
import com.vz.backend.business.repository.IOrgDocBookRepository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Constant;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;
import com.vz.backend.core.service.CategoryService;
import org.springframework.transaction.annotation.Transactional;

/**
 * @author DucND
 * @date May 29, 2020
 */
@Service
public class DocumentBookService extends BaseService<DocumentBook> {
	@Value("${configs.doc-book.org_config: false}")
	private boolean orgConfig;

	@Value("${configs.clerical-org: false}")
	private boolean clericalOrg;

	@Autowired
	IDocumentBookRepository documentBookRepository;

	@Autowired
	IOrgDocBookRepository orgDocBookRepo;

	@Autowired
	EntityManagerFactory entityManagerFactory;

	@Autowired
	private CategoryService categoryService;
	
	@Autowired 
	private CategoryDocBookService categoryDocBookService;
	
	@Autowired
	private ClericalOrgService clericalOrgService;
	
	@Autowired
	private OrgDocBookService orgDocBookService;

	@Override
	public IRepository<DocumentBook> getRepository() {
		return documentBookRepository;
	}

	public DocumentBook findByBookId(Long bookId) {
		Optional<DocumentBook> item = documentBookRepository.findById(bookId);
		if (!item.isPresent()) {
			throw new RestExceptionHandler(Message.DOCUMENT_BOOK_NOT_FOUND);
		}
		return item.get();
	}

	public Page<DocumentBookDto> findPageByOrgIdAndClientId(Pageable pageable) {
		Page<DocumentBookDto> result;
		if (orgConfig) {
			result = documentBookRepository.findDBByOrgIdAndClientId(BussinessCommon.getUser().getOrg(),
					BussinessCommon.getClientId(), pageable);
		} else {
			result = documentBookRepository.findDBByClientId(BussinessCommon.getClientId(), pageable);
		}
		fillOrgIds(result.getContent());
		return result;
	}

	private void fillOrgIds(List<DocumentBookDto> content) {
		for (DocumentBookDto db : content) {
			db.setOrgIds(orgDocBookRepo.findOrgIdByBookIdAndActive(db.getId(), true));
		}
	}

	public List<DocumentBook> findByActiveAndBookType(Long typeId, Boolean active) {
		if (orgConfig) {
			List<Long> clericalOrgs = new ArrayList<>();
			if (clericalOrg) {
				clericalOrgs.addAll(clericalOrgService.getClericalOrg(BussinessCommon.getUserId()));
			} else {
				clericalOrgs.add(BussinessCommon.getOrgId());
			}
			return documentBookRepository.findByOrgIdAndBookTypeAndActiveAndClientId(
					clericalOrgs, typeId, active, BussinessCommon.getClientId());
		}
		return documentBookRepository.findByActiveAndBookType(typeId, active, BussinessCommon.getClientId());
	}

	public Page<DocumentBookDto> searchDocumentBook(String name, Long typeId, Integer year, Boolean active,
			Pageable pageable) {
		Page<DocumentBookDto> result;
		if (orgConfig) {
			result = documentBookRepository.searchDocumentBookWithOrgId(BussinessCommon.getUser().getOrg(), BussinessCommon.convert(name),
					typeId, year, active, BussinessCommon.getClientId(), pageable);
		} else {
			result = documentBookRepository.searchDocumentBook(BussinessCommon.convert(name), typeId, year, active,
					BussinessCommon.getClientId(), pageable);
		}
		fillOrgIds(result.getContent());
		return result;
	}

	public Long getMaxCurrentNumberByBookType(Long typeId) {
		Long max = documentBookRepository.getMaxCurrentNumberByBookType(typeId, BussinessCommon.getClientId());
		return max != null ? max : 0;
	}

	public List<DocumentBookInitDto> castToCategoryInitDto(List<DocumentBook> dbList) {
		if (dbList == null) {
			return null;
		}
		List<DocumentBookInitDto> dtoList = new ArrayList<>();
		dbList.stream().forEach(c -> {
			DocumentBookInitDto dto = new DocumentBookInitDto();
			dto.setId(c.getId());
			dto.setName(c.getName() + " - " + c.getYear());
			dto.setCurrentNumber(c.getCurrentNumber());
			dto.setNumberOrSign(c.getNumberOrSign());
			dto.setValue(c.getCurrentNumber());
			dto.setYear(c.getYear());
			dto.setSecurityIds(documentBookRepository.getListSecurityIdsByBookIdAndClientId(c.getId(), BussinessCommon.getClientId()));
			dtoList.add(dto);
		});
		return dtoList;
	}

	public List<DocumentBook> findByBookType(Long typeCode) {
		if (orgConfig) {
			return documentBookRepository.findByBookTypeAndOrgIdAndClientId(BussinessCommon.getUser().getOrg(),
					typeCode, BussinessCommon.getClientId());
		}
		return documentBookRepository.findByBookTypeAndClientId(typeCode, BussinessCommon.getClientId());
	}

	public List<DocumentBook> findByBookTypeFlowing(Long typeCode) {

			return documentBookRepository.findByBookTypeAndOrgIdAndClientId(2L,
					typeCode, BussinessCommon.getClientId());


	}

	@Transactional
	public void lockDocumentBook() {
		int year = Calendar.getInstance().get(Calendar.YEAR);
		documentBookRepository.lockDocumentBook(year);
	}

	public void updateCurrentNumber(Long bookId, Long number) {
		if (number != null) {
			DocumentBook db = findByBookId(bookId);
			if (number > db.getCurrentNumber()) {
				db.setCurrentNumber(number);
				documentBookRepository.save(db);
			}
		}
	}

	public boolean add(DocumentBookWithListOrgIds input) {
		input.setId(null);
		DocumentBook db = documentBookRepository.save(input.getDb());
		//Add org
		orgDocBookService.add(db.getId(), input.getOrgIds());
		//Add category
		categoryDocBookService.add(db.getId(), input.getCategoryIds());
		return true;
	}

	public boolean update(Long id, DocumentBookWithListOrgIds input) {
		Optional<DocumentBook> oData = documentBookRepository.findById(id);
		DocumentBook db = null;
		if (oData.isPresent()) {
			db = oData.get();
			if (orgConfig && !db.getOrgCreateId().equals(BussinessCommon.getUser().getOrg())) {
				throw new RestExceptionHandler(Message.DOCUMENT_BOOK_UPDATE);
			}
			if (input.getDb().getActive() != null) {
				db.setActive(input.getDb().getActive());
			}
			if (input.getDb().getBookType() != null) {
				db.setBookType(input.getDb().getBookType());
			}
			if (input.getDb().getName() != null) {
				db.setName(input.getDb().getName());
			}
			if (input.getDb().getNumberOrSign() != null) {
				db.setNumberOrSign(input.getDb().getNumberOrSign());
			}
			if (input.getDb().getYear() != null) {
				db.setYear(input.getDb().getYear());
			}
			if (input.getDb().getCurrentNumber() != null) {
				db.setCurrentNumber(input.getDb().getCurrentNumber());
			}
			db = documentBookRepository.save(db);
			//Update org
			orgDocBookService.add(db.getId(), input.getOrgIds());
			//Update category
			categoryDocBookService.add(db.getId(), input.getCategoryIds());
		} else {
			throw new RestExceptionHandler(Message.DOCUMENT_BOOK_NOT_FOUND);
		}
		return true;
	}
	
	public DocumentBookDetailDto getDetailById(Long id) {
		DocumentBookDetailDto result = documentBookRepository.getDetailById(id, BussinessCommon.getClientId());
		if (result == null) throw new RestExceptionHandler(Message.DOCUMENT_BOOK_NOT_FOUND);
		result.setOrgIds(orgDocBookService.findOrgIdByBookIdAndActive(id, true));
		result.setCategoryIds(categoryDocBookService.findCategoryIdByBookIdAndActive(id, true));
		if (orgConfig && !(result.getOrgId().equals(BussinessCommon.getUser().getOrg()) || result.getOrgIds().contains(BussinessCommon.getOrgId()))) {
			throw new RestExceptionHandler(Message.NO_PERMISSION);
		}
		return result;
	}
	
	public List<DocumentBookInitDto> getMaxNumberVBDen() {
		List<DocumentBookInitDto> result = new ArrayList<>();
		List<DocumentBook> listDB;
		listDB = findByActiveAndBookType(Constant.BOOK_DOC_VB_DEN, true);
		for (DocumentBook db : listDB) {
			DocumentBookInitDto temp = new DocumentBookInitDto();
			temp.setId(db.getId());
			temp.setName(db.getName() + " - " + db.getYear());
			temp.setYear(db.getYear());
			temp.setCurrentNumber(db.getCurrentNumber());
			temp.setValue(db.getCurrentNumber() == null ? 1 : db.getCurrentNumber());
			temp.setNumberOrSign(db.getNumberOrSign());
			List<Long> securityIds = documentBookRepository.getListSecurityIdsByBookIdAndClientId(db.getId(), BussinessCommon.getClientId());
			if(!securityIds.isEmpty()) {
				temp.setSecurityIds(securityIds);
				result.add(temp);
			}
		}
		return result;
	}
}
