package com.vz.backend.business.service.hstl;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.vz.backend.business.config.FolderPermissionEnum;
import com.vz.backend.business.controller.DocumentOutController.GroupBy;
import com.vz.backend.business.domain.hstl.HsFolderDocument;
import com.vz.backend.business.dto.fullreport.ReportKey;
import com.vz.backend.business.dto.hstl.DocumentByMonth;
import com.vz.backend.business.dto.hstl.DocumentReportDto;
import com.vz.backend.business.dto.hstl.FolderDetailBasicDto;
import com.vz.backend.business.dto.hstl.export.ContentDoc;
import com.vz.backend.business.repository.hstl.IHsFolderDocumentRepository;
import com.vz.backend.business.service.DocumentOutService;
import com.vz.backend.business.service.DocumentService;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;
import com.vz.backend.util.DateTimeUtils;

@Service
public class HsFolderDocumentService extends BaseService<HsFolderDocument> {

	@Autowired
	private IHsFolderDocumentRepository hsFolderDocRepo;
	@Autowired
	private HsFolderService hsFolderService;
	
	@Autowired
	private DocumentService docInService;
	
	@Autowired
	private DocumentOutService docOutService;

	@Override
	public IRepository<HsFolderDocument> getRepository() {
		return hsFolderDocRepo;
	}

	public Boolean add(Long folderId, List<HsFolderDocument> input) {
		hsFolderService.validatePermission(folderId, FolderPermissionEnum.FULL, true);
		int count = 0;
		HsFolderDocument temp;
		for (HsFolderDocument fd : input) {
			if(folderId == null) {
				//check exist
				isExistDocIdAndTypeAndTypeAndFolderIdIsNull(fd.getDocId(), fd.getType());
			}
			
			if(DocumentTypeEnum.VAN_BAN_DEN.equals(fd.getType())) {
				temp = new HsFolderDocument(docInService.valid(fd.getDocId(), Message.NOT_FOUND_DOC), folderId, fd.getComment());
			} else {
				temp = new HsFolderDocument(docOutService.valid(fd.getDocId(), Message.NOT_FOUND_DOC), folderId, fd.getComment());
			}
			
			try {
				fd = hsFolderDocRepo.save(temp);
			} catch (Exception e) {
				count++;
				continue;
			}
			// increase totalItems for parent folder
			hsFolderService.increaseTotalItems(fd.getFolderId(), 1L);
		}
		if (count > 0)
			throw new RestExceptionHandler("Có " + count + " văn bản đã tồn tại trong hồ sơ");
		return true;
	}

	public Boolean deleteDocumentById(Long id) {
		try {
			HsFolderDocument doc = hsFolderDocRepo.findByClientIdAndId(BussinessCommon.getClientId(), id);
			if (doc == null) throw new RestExceptionHandler(Message.ACTION_FAILED);
			hsFolderService.validatePermission(doc.getFolderId(), FolderPermissionEnum.FULL, true);
			hsFolderDocRepo.deleteById(id);
			hsFolderService.decreaseTotalItems(doc.getFolderId(), 1L);
		} catch (Exception e) {
			e.printStackTrace();
			throw new RestExceptionHandler(Message.ACTION_FAILED);
		}
		return true;
	}

	public List<DocumentByMonth> report(Date startDate, Date endDate, GroupBy groupBy) {
		try {
			// don't need check client id (check user id)
			List<DocumentReportDto> report = hsFolderDocRepo.report(startDate, endDate, BussinessCommon.getUserId());

			Map<String, DocumentByMonth> docMap = new HashMap<>();
			for (DocumentReportDto r : report) {
				getDby(docMap, r.getCreateDate(), groupBy.getMonth()).add(r.getId(), r.getDocType());
			}
			fillEmptyPart(docMap, report, groupBy.getMonth(), startDate, endDate);

			List<DocumentByMonth> result = new ArrayList<>(docMap.values());
			result.sort((a, b) -> a.getKey().compareTo(b.getKey()));
			return result;
		} catch (Exception e) {
			e.printStackTrace();
			return new ArrayList<>();
		}
	}

	private DocumentByMonth getDby(Map<String, DocumentByMonth> map, Date time, int groupMonth) {
		ReportKey key = DocumentReportDto.getKey(time, groupMonth);
		if (!map.containsKey(key.getKey())) {
			map.put(key.getKey(), new DocumentByMonth(key));
		}
		return map.get(key.getKey());
	}

	private void fillEmptyPart(Map<String, DocumentByMonth> map, List<DocumentReportDto> report, int groupMonth,
			Date startDate, Date endDate) {

		Date minDate = startDate;
		Date maxDate = endDate;
		for (DocumentReportDto r : report) {
			Date currDate = r.getCreateDate();
			if (minDate == null || minDate.compareTo(currDate) > 0) {
				minDate = currDate;
			}
			if (maxDate == null || maxDate.compareTo(currDate) < 0) {
				maxDate = currDate;
			}
		}
		if (minDate != null && maxDate != null) {
			Calendar cal = Calendar.getInstance(DateTimeUtils.timeZone());
			cal.setTime(minDate);
			cal.add(Calendar.MILLISECOND, 1);
			cal.set(Calendar.DAY_OF_MONTH, 1);
			Date now = cal.getTime();

			while (now.compareTo(maxDate) < 0) {
				getDby(map, now, groupMonth);
				cal.set(Calendar.MONTH, cal.get(Calendar.MONDAY) + groupMonth);
				now = cal.getTime();
			}
		}
	}

	public void requireHsForDocument(DocumentTypeEnum docType, Long docId) {
		if (true) {
			return;
		}
		Long userId = BussinessCommon.getUserId();
		if (!hsFolderDocRepo.existsByCreateByAndTypeAndDocIdAndActiveTrue(userId, docType, docId)) {
			throw new RestExceptionHandler(Message.NO_HS);
		}
	}

	public Long countHoSoByDocIdAndDocType(Long docId, DocumentTypeEnum docType) {
		return hsFolderDocRepo.countByDocIdAndTypeAndActiveAndClientId(docId, docType, true, BussinessCommon.getClientId());
	}

	public List<FolderDetailBasicDto> getHosoByDocIdAndDocType(Long docId, DocumentTypeEnum docType) {
		return hsFolderDocRepo.getHosoByDocIdAndDocType(BussinessCommon.getUserId(), docId, docType, BussinessCommon.getClientId());
	}
	
	public List<ContentDoc> getByFolderId(Long folderId) {
		return hsFolderDocRepo.getByFolderId(folderId, BussinessCommon.getClientId());
	}
	
	private void isExistDocIdAndTypeAndTypeAndFolderIdIsNull(Long docId, DocumentTypeEnum type) {
		long count = hsFolderDocRepo.countByDocIdAndTypeAndFolderIdIsNull(docId, type, BussinessCommon.getClientId());
		if (count > 0) {
			throw new RestExceptionHandler("Văn bản được chọn đã tồn tại trong hồ sơ");
		}
	}
}
