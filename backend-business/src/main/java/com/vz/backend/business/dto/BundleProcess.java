package com.vz.backend.business.dto;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.springframework.lang.NonNull;

import com.vz.backend.business.domain.DocumentOut;
import com.vz.backend.business.domain.DocumentOutAttachment;
import com.vz.backend.business.domain.DocumentOutProcess;
import com.vz.backend.core.domain.Category;
import com.vz.backend.core.domain.User;

import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class BundleProcess {
	private Map<Long, DocumentOut> mapDocumentOut = new HashMap<>();
	private Map<Long, User> mapUser = new HashMap<>();
	private Map<Long, Category> mapCategory = new HashMap<>();
	@Getter
	private List<Long> dopList = new ArrayList<>();
	private Map<Long, List<DocumentOutAttachment>> mapAtt = new HashMap<>();

	public enum MapType {
		DOCUMENT_OUT, USER, CATEGORY;
	}

	public BundleProcess(List<DocumentOutProcess> processes) {
		for (DocumentOutProcess process : processes) {
			dopList.add(process.getDocId());
			this.mapDocumentOut.put(process.getDocId(), null);
			this.mapUser.put(process.getUserId(), null); // handleUser
		}
	}

	@NonNull
	public Set<Long> getKeys(@NonNull MapType type) {
		switch (type) {
		case DOCUMENT_OUT:
			return mapDocumentOut.keySet();
		case USER:
			return mapUser.keySet();
		case CATEGORY:
			return mapCategory.keySet();
		default:
			return new HashSet<>();
		}
	}

	public void initDocument(List<DocumentOut> documentOutList) {
		for (DocumentOut documentOut : documentOutList) {
			this.mapDocumentOut.put(documentOut.getId(), documentOut);

			this.mapUser.put(documentOut.getPersonEnterId(), null); // userEnter

			this.mapCategory.put(documentOut.getDocTypeId(), null);
			this.mapCategory.put(documentOut.getSecurityId(), null);
		}

	}

	public void initUser(List<User> userList) {
		for (User user : userList) {
			this.mapUser.put(user.getId(), user);
		}
	}

	public void initCategory(List<Category> categoryList) {
		for (Category category : categoryList) {
			this.mapCategory.put(category.getId(), category);
		}
	}

	public DocumentOut getDocumentOut(Long docId) {
		DocumentOut documentOut = this.mapDocumentOut.get(docId);
		if (documentOut == null) {
			log.error("Not exist doc id: {}", docId);
		}
		return documentOut;
	}

	public Category getCategory(Long cateId) {
		Category category = this.mapCategory.get(cateId);
		if (category == null) {
			log.error("Not exist category id: {}", cateId);
		}
		return category;
	}

	public User getUser(Long userId) {
		User user = this.mapUser.get(userId);
		if (user == null) {
			log.error("Not exist user id: {}", userId);
		}
		return user;
	}

	public List<DocumentOutAttachment> getAttByDocId(Long docId) {
		List<DocumentOutAttachment> doaList = mapAtt.get(docId);
		if (doaList == null) {
			log.error("Not exist docId: {}", docId);
			return new ArrayList<>();
		}
		return doaList;
	}

	public void setListAtt(List<DocumentOutAttachment> doaList) {
		for (DocumentOutAttachment doa : doaList) {
			Long docId = doa.getDocId();
			if (!mapAtt.containsKey(docId)) {
				mapAtt.put(docId, new ArrayList<>());
			}
			mapAtt.get(docId).add(doa);
		}
	}
}
