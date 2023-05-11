package com.vz.backend.business.service.hstl;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.ResourceUtils;
import org.wickedsource.docxstamper.DocxStamper;
import org.wickedsource.docxstamper.DocxStamperConfiguration;

import com.vz.backend.business.domain.hstl.Headings;
import com.vz.backend.business.domain.hstl.HsFolder;
import com.vz.backend.business.dto.calendar.Calendar2Part;
import com.vz.backend.business.dto.hstl.FolderTree;
import com.vz.backend.business.dto.hstl.export.ContentDoc;
import com.vz.backend.business.dto.hstl.export.ContentFolders;
import com.vz.backend.business.dto.hstl.export.ExportDocs;
import com.vz.backend.business.dto.hstl.export.ExportFolders;
import com.vz.backend.business.dto.hstl.export.ExportHeadings;
import com.vz.backend.business.repository.hstl.IHeadingsRepoository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.AuthorityEnum;
import com.vz.backend.core.config.Constant;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.domain.Category;
import com.vz.backend.core.domain.Organization;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.AuthorityUserService;
import com.vz.backend.core.service.BaseService;
import com.vz.backend.core.service.CategoryService;
import com.vz.backend.core.service.OrganizationService;
import com.vz.backend.util.DateTimeUtils;

@Service
public class HeadingsService extends BaseService<Headings> {

	@Autowired
	private HsFolderService hsFolderService;

	@Autowired
	private IHeadingsRepoository headingsRepository;

	@Autowired
	private AuthorityUserService authorityService;
	
	@Autowired
	private CategoryService categoryService;
	
	@Autowired
	private HsFolderDocumentService folderDocService;
	
	@Autowired
	private HsFolderFileService folderFileService;
	
	@Autowired
	private OrganizationService orgService;

	@Override
	public IRepository<Headings> getRepository() {
		return headingsRepository;
	}

	public static final String BREAK = ".";

	private void validParentId(Headings f) {
		Set<Long> gens = new HashSet<>();
		if (f.getParentId() == null) {
			return;
		}
		gens = getNextGen(f.getId(), new HashSet<>());
		if (gens.contains(f.getParentId())) {
			throw new RestExceptionHandler(Message.INVALID_HEADINGS);
		}
	}

	public Set<Long> getNextGen(Long id, Set<Long> set) {
		set.add(id);
		List<Long> ids = findIdByParentId(id);
		if (ids.isEmpty()) {
			return set;
		}
		for (Long i : ids) {
			getNextGen(i, set);
		}

		return set;
	}

	private List<Headings> findByParentId(Long id) {
		return headingsRepository.findByParentIdAndClientIdAndActiveTrue(id, BussinessCommon.getClientId());
	}

	private List<Long> findIdByParentId(Long id) {
		return findByParentId(id).stream().map(Headings::getId).collect(Collectors.toList());
	}

	public Headings update(Long id, Headings input) {
		Headings old = valid(id, Message.NOT_FOUND_OBJECT);
		validParentId(input);
		old.set(input);
		try {
			return headingsRepository.save(old);
		} catch (Exception e) {
			throw new RestExceptionHandler(Message.WRONG_INPUT_DATA);
		}
	}
	
	public void validRole() {
		if(!authorityService.isUserHasAuthority(BussinessCommon.getUserId(), null, AuthorityEnum.MANAGE_HEADINGS)) {
			throw new RestExceptionHandler(Message.INVALID_ROLE_MANAGE_HEADINGS);
		}
	}

	public List<Headings> getHeadingsTree() {
		List<Headings> all = headingsRepository.findByClientIdAndActive(BussinessCommon.getClientId(), true);
		List<Headings> roots = all.stream().filter(i -> i.getParentId() == null).collect(Collectors.toList());
		all.removeAll(roots);
		pushTree(roots, all);
		return roots;
	}

	private void pushTree(List<Headings> parents, List<Headings> childrens) {
		if(parents.isEmpty()) {
			return;
		}
		for (Headings i : parents) {
			setSub(i, childrens);
			pushTree(i.getChildren(), childrens);
		}
	}

	private void setSub(Headings headings, List<Headings> all) {
		List<Headings> subs = new ArrayList<>();
		if (all.isEmpty()) {
			return;
		}
		for (Headings i : all) {
			if (headings.getId().equals(i.getParentId())) {
				subs.add(i);
			}
		}
		headings.setChildren(subs);
		all.removeIf(i -> subs.contains(i));
	}

	//push folders tree into headings tree
	public List<ContentFolders> getRecordHeadings(String text, Integer yearFolders, String typeFolders,
			Long maintenance, Date from, Date to) {
		text = BussinessCommon.convert(text);
		typeFolders = BussinessCommon.convert(typeFolders);
		from = DateTimeUtils.handleSubmit(from);
		to = DateTimeUtils.getEndDate(to);
		getTmpOrg();
		List<HsFolder> folders = hsFolderService.getListApprove(text, yearFolders, typeFolders, maintenance, from, to, this.tmpOrgId);
		List<Headings> headings = getFloatHeadings(folders, text, yearFolders, typeFolders);
		return ContentFolders.convert(treeToList(headings), hsFolderService.treeToList(folders, true));
	}

	// get data corresponding branch root
	private List<Headings> treeToList(List<Headings> headings) {
		List<Headings> rs = new ArrayList<>();
		List<Headings> childs;
		List<Headings> tree = buildHeadingTree(headings);
		setArticleHeadings(tree);
		for (Headings i : tree) {
			childs = i.getChildren();
			i.setChildren(null);
			rs.add(i);
			rs.addAll(level(childs, new ArrayList<>()));
		}
		return rs;
	}

	private List<Headings> level(List<Headings> headings, List<Headings> rs) {
		if (headings.isEmpty()) {
			return rs;
		}
		List<Headings> childs = new ArrayList<>();
		for (Headings i : headings) {
			childs = i.getChildren();
			if (!rs.contains(i)) {
				i.setChildren(null);
				rs.add(i);
			}

			level(childs, rs);
		}
		return rs;
	}

	public void export(OutputStream outputStream, String text, Integer yearFolders, String typeFolders,
			Long maintenance, Date from, Date to) {
		ExportHeadings dto = new ExportHeadings(
				getRecordHeadings(text, yearFolders, typeFolders, maintenance, from, to));
		try {
			DocxStamperConfiguration stamperConfig = new DocxStamperConfiguration();
			stamperConfig.setLineBreakPlaceholder(Calendar2Part.BREAK_LINE);
			@SuppressWarnings("unchecked")
			DocxStamper<ExportHeadings> stamper = stamperConfig.build();
			File fs = ResourceUtils.getFile("classpath:templates/Phiếu danh mục hồ sơ.docx");
			stamper.stamp(new FileInputStream(fs), dto, outputStream);
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			try {
				if (outputStream != null) {
					outputStream.close();
				}
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}

	public Boolean del(Long id) {
		valid(id, Message.NOT_FOUND_OBJECT);
		try {
			deleteById(id);
		} catch (Exception e) {
			throw new RestExceptionHandler(Message.INVALID_DELETE_HEADINGS);
		}
		return true;
	}

	/**
	 * Create headings, folders tree base on result found by conditions
	 * @param text
	 * @param yearFolders
	 * @param typeFolders
	 * @param byUnit
	 * @return
	 */
	public List<Headings> buildHeadingsFolderTree(String text, Integer yearFolders, String typeFolders, Long maintenance,
			Date from, Date to) {
		getTmpOrg();
		List<HsFolder> folders = hsFolderService.getListApprove(text, yearFolders, typeFolders, maintenance, from, to, this.tmpOrgId);
		List<Headings> headings = getFloatHeadings(folders, text, yearFolders, typeFolders);
		pushFolderToHeadings(headings, hsFolderService.buildTree(folders));
		return buildHeadingTree(headings);
	}

	/**
	 * Get headings list by text and folders
	 * @param folders
	 * @param text
	 * @param byUnit 
	 * @param typeFolders 
	 * @param yearFolders 
	 * @return
	 */
	private List<Headings> getFloatHeadings(List<HsFolder> folders, String text, Integer yearFolders, String typeFolders) {
		// Targets
		// 1. Get list all headings -> filter by conditions
		// 2. Get list headings from corresponding folders
		// 3. Merge into float headings list
		
		boolean check = yearFolders != null || typeFolders != null;
		List<Headings> headings = findByClientIdAndActive(BussinessCommon.getClientId(), true);
		List<Headings> floats = new ArrayList<>();
		List<Headings> byText = new ArrayList<>();
		if (!check) {
			byText = headings.stream().filter(i -> (text == null || i.getName().toLowerCase().contains(text)))
					.collect(Collectors.toList());
		}
		
		List<Headings> byFolders = folders.stream().map(HsFolder::getHeadingsObj).filter(i->Boolean.TRUE.equals(i.getActive())).distinct()
				.collect(Collectors.toList());
		floats.addAll(byText);
		floats.addAll(byFolders);
		floats.stream().distinct().collect(Collectors.toList());
		return floats;
	}

	private List<Headings> pushFolderToHeadings(List<Headings> headings, List<FolderTree> folders) {
		List<FolderTree> fRoots;
		for (Headings h : headings) {
			fRoots = new ArrayList<>();
			for (FolderTree f : folders) {
				if (f.getHeadingsId() == null) {
					continue;
				}
				if (h.getId().equals(f.getHeadingsId()) && f.getParentId() == null) {
					fRoots.add(f);
				}
			}
			h.setHsFolders(fRoots);
		}
		return headings;
	}

	private List<Headings> buildHeadingTree(List<Headings> floats) {
		Map<Long, Headings> pMap = new HashMap<>();
		List<Headings> rs = new ArrayList<>();
		Headings tmp;
		for (Headings f : floats) {
			tmp = buildHeadingTree(f, pMap);//root
			if(rs.contains(tmp)) {
				rs.remove(tmp);
			}
			rs.add(tmp);
		}

		return rs;
	}

	private Headings buildHeadingTree(Headings child, Map<Long, Headings> pMap) {
		Long key = child.getId();
		if (!pMap.containsKey(key)) {
			pMap.put(key, child);
		}

		Headings tmp = getPHeadings(child);
		if (tmp == null) {
			return child;
		}

		if(!tmp.getChildren().contains(child)) {
			tmp.getChildren().add(child);
		}
		return buildHeadingTree(tmp, pMap);
	}

	private Headings getPHeadings(Headings f) {
		if (f.getParentId() == null) {
			return null;
		}

		if (f.getParent() == null) {
			return valid(f.getParentId(), Message.NOT_FOUND_OBJECT);
		}

		return f.getParent();
	}

	private void setArticleHeadings(List<Headings> fRoots) {
		int count = 0;
		for (Headings i : fRoots) {
			if(i.getParentId() == null) {
				count ++;
			}
			i.setArticle(BussinessCommon.integerToRoman(count));
			i.setName(i.getArticle() + BREAK + " " + i.getName());
			setArticleHeadingChild(i, new HashMap<>());
		}
	}

	private void setArticleHeadingChild(Headings parent, Map<Long, Integer> map) {
		List<Headings> childrens = parent.getChildren();
		if (childrens.isEmpty()) {
			return;
		}
		Long key = parent.getId();
		int count = 1;
		String article = "";
		for (Headings i : childrens) {
			if (map.containsKey(key)) {
				count = map.get(key) + 1;
			}
			map.put(key, count);
			if (parent.getParentId() != null) {
				article = parent.getArticle() + HeadingsService.BREAK;
			}

			i.setArticle(article + count);
			i.setName(i.getArticle() + BREAK + " " + i.getName());
			setArticleHeadingChild(i, map);
		}
	}
	
	private List<ContentFolders> getFolders(String text, Integer yearFolders, String typeFolders,
			Long maintenance, Date from, Date to) {
		text = BussinessCommon.convert(text);
		typeFolders = BussinessCommon.convert(typeFolders);
		from = DateTimeUtils.handleSubmit(from);
		to = DateTimeUtils.getEndDate(to);
		List<HsFolder> folders = hsFolderService.getListApprove(text, yearFolders, typeFolders, maintenance, from, to, this.tmpOrgId);
		return ContentFolders.convert(hsFolderService.treeToList(folders, false));
	}

	private boolean unLimitFolders(Long maintenance) {
		if (maintenance == null || maintenance.longValue() == 0) {
			return false;
		}

		Category maintenanceObj = categoryService.valid(maintenance, Message.NOT_FOUND_CATEGORY);
		return Boolean.TRUE.equals(maintenanceObj.getIsDefault());
	}
	
	private Long tmpOrgId = null;
	private String tmpOrgName = null;
	private Organization getTmpOrg() {
		Organization org = BussinessCommon.getUser().getOrgModel();
		if(Constant.PHONG.equalsIgnoreCase(org.getOrgTypeModel().getName())) {
			Long orgId = orgService.getParentByOrgType(BussinessCommon.getUser().getOrgModel(), Constant.CUC_VU_VIEN);
			org = orgService.valid(orgId, Message.NOT_FOUND_ORG);
		}
		this.tmpOrgId = org.getId();
		this.tmpOrgName = org.getName();
		return org;
	}
	
	private ExportFolders getExportFolders(String text, Integer yearFolders, String typeFolders,
			Long maintenance, Date from, Date to) {
		getTmpOrg();
		ExportFolders dto = new ExportFolders(
				getFolders(text, yearFolders, typeFolders, maintenance, from, to), unLimitFolders(maintenance), tmpOrgName);
		
		return dto;
	}
	
	public void exportFolders(OutputStream outputStream, String text, Integer yearFolders, String typeFolders,
			Long maintenance, Date from, Date to) {
		ExportFolders dto = getExportFolders(text, yearFolders, typeFolders, maintenance, from, to);
		try {
			DocxStamperConfiguration stamperConfig = new DocxStamperConfiguration();
			stamperConfig.setLineBreakPlaceholder(Calendar2Part.BREAK_LINE);
			@SuppressWarnings("unchecked")
			DocxStamper<ExportFolders> stamper = stamperConfig.build();
			File fs = ResourceUtils.getFile("classpath:templates/Phiếu mục lục hồ sơ, tài liệu nộp lưu.docx");
			stamper.stamp(new FileInputStream(fs), dto, outputStream);
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			try {
				if (outputStream != null) {
					outputStream.close();
				}
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}
	
	private List<ContentDoc> getByFolder(Long folderId) {
		List<ContentDoc> rs = new ArrayList<>();
		List<ContentDoc> docs = folderDocService.getByFolderId(folderId);
		ContentDoc.setNumber(docs, 0);
		List<ContentDoc> files = folderFileService.getByFolderId(folderId);
		ContentDoc.setNumber(files, docs.size() + 1);
		rs.addAll(files);
		rs.addAll(docs);
		return rs;
	}
	
	public void exportDocs(OutputStream outputStream, Long folderId) {
		HsFolder f = hsFolderService.valid(folderId, Message.NOT_FOUND_OBJECT);
		ExportDocs dto = new ExportDocs(getByFolder(folderId), f.getFileNotation());
		try {
			DocxStamperConfiguration stamperConfig = new DocxStamperConfiguration();
			stamperConfig.setLineBreakPlaceholder(Calendar2Part.BREAK_LINE);
			@SuppressWarnings("unchecked")
			DocxStamper<ExportDocs> stamper = stamperConfig.build();
			File fs = ResourceUtils.getFile("classpath:templates/Phiếu mục lục văn bản, tài liệu.docx");
			stamper.stamp(new FileInputStream(fs), dto, outputStream);
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			try {
				if (outputStream != null) {
					outputStream.close();
				}
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}
}
