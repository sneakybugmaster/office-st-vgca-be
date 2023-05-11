package com.vz.backend.business.service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.vz.backend.business.domain.AttachmentVersion;
import com.vz.backend.business.domain.DocumentOutAttachment;
import com.vz.backend.business.repository.IAttachmentVersionRepository;
import com.vz.backend.business.repository.IDocumentOutAttachmentRepository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.AttachmentTypeEnum;
import com.vz.backend.core.service.FilesStorageService;
import org.springframework.web.multipart.MultipartFile;

@Service
public class AttachmentVersionService {

	@Autowired
	IAttachmentVersionRepository attachVersionRepository;

	@Autowired
	FilesStorageService storageService;

	@Autowired
	IDocumentOutAttachmentRepository docOutAttachRepository;

	public List<AttachmentVersion> addNewAttachmentVersion(List<DocumentOutAttachment> listDocOutAttach,
			boolean isFirst) {
		if (BussinessCommon.isEmptyList(listDocOutAttach)) {
			return Collections.emptyList();
		}
		
		// filter user version attachment with non encrypt file
		listDocOutAttach = listDocOutAttach.stream().filter(i -> !Boolean.TRUE.equals(i.getEncrypt()))
				.collect(Collectors.toList());
		List<AttachmentVersion> listAttachVersion = new ArrayList<>();
		for (DocumentOutAttachment docOutAttach : listDocOutAttach) {
			String fileName = docOutAttach.getName();
			int indexDot = 0;
			if ((isFirst || docOutAttach.getIsChanged() != null && docOutAttach.getIsChanged())
					&& docOutAttach.getAttachmentType() == AttachmentTypeEnum.DRAFT) {
				Integer maxVersion = attachVersionRepository.getMaxVersionByDocId(docOutAttach.getDocId(), BussinessCommon.getClientId());
				if (maxVersion == null) {
					maxVersion = 1;
					indexDot =fileName.lastIndexOf(".");
				} else {
					maxVersion += 1;
					indexDot =fileName.lastIndexOf("_v");
					if(indexDot == -1){
						indexDot =fileName.lastIndexOf(".");
					}
				}
				fileName = fileName.substring(0, indexDot) + "_v" + maxVersion + fileName.substring(fileName.lastIndexOf("."), fileName.length());

				storageService.copyFile(docOutAttach.getName(), fileName);

				AttachmentVersion a = new AttachmentVersion();
				a.setName(fileName);
				a.setOriginName(docOutAttach.getName());
				a.setType(docOutAttach.getType());
				a.setSize(docOutAttach.getSize());
				a.setDocId(docOutAttach.getDocId());
				a.setVersion(maxVersion);
				a.setUserFullName(BussinessCommon.getUser().getFullName());
				attachVersionRepository.save(a);
				listAttachVersion.add(a);
				docOutAttach.setIsChanged(false);
				docOutAttachRepository.save(docOutAttach);
			}
		}
		return listAttachVersion;
	}

	public void addFirstVersion(MultipartFile file, Long objId, String originName) {
		try {
			Integer maxVersion = 0;
			Integer indexDot = 0;
			indexDot = originName.lastIndexOf(".");
			String fileName = originName.substring(0, indexDot) + "_v" + maxVersion + originName.substring(originName.lastIndexOf("."), originName.length());
			AttachmentVersion a = new AttachmentVersion();
			a.setName(originName);
			a.setOriginName(fileName);
			a.setType(file.getContentType());
			a.setSize(file.getSize());
			a.setDocId(objId);
			a.setVersion(maxVersion);
			a.setUserFullName(BussinessCommon.getUser().getFullName());
			attachVersionRepository.save(a);
		} catch (Exception e) {

		}
	}

	public List<AttachmentVersion> findAllByDocId(Long docId) {
		return attachVersionRepository.findAllByDocId(docId, BussinessCommon.getClientId());
	}
}
