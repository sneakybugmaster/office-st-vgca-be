package com.vz.backend.business.service.hstl;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.ResourceUtils;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;
import org.wickedsource.docxstamper.DocxStamper;
import org.wickedsource.docxstamper.DocxStamperConfiguration;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;
import com.vz.backend.business.domain.TaskAttachment;
import com.vz.backend.business.domain.hstl.HsFolder;
import com.vz.backend.business.domain.hstl.HsFolderRecord;
import com.vz.backend.business.domain.hstl.HsFolderRecordForm;
import com.vz.backend.business.dto.calendar.Calendar2Part;
import com.vz.backend.business.dto.hstl.ecm.FolderAttachmentDto;
import com.vz.backend.business.dto.hstl.ecm.FolderStatusDto;
import com.vz.backend.business.dto.hstl.ecm.FolderStatusListDto;
import com.vz.backend.business.dto.hstl.ecm.FormRegisterDto;
import com.vz.backend.business.dto.hstl.ecm.HsFolderRecordDto;
import com.vz.backend.business.dto.hstl.ecm.HsFolderRecordEcmDto;
import com.vz.backend.business.dto.hstl.ecm.HsFolderRecordFormDto;
import com.vz.backend.business.dto.hstl.ecm.HsFolderRecordFormListDto;
import com.vz.backend.business.dto.hstl.ecm.HsFolderRecordListDto;
import com.vz.backend.business.dto.hstl.ecm.UpdateStatusRegisterFormDto;
import com.vz.backend.business.dto.hstl.export.ExportFolderRecordForm;
import com.vz.backend.business.repository.hstl.IHsFolderRecordFormRepository;
import com.vz.backend.business.repository.hstl.IHsFolderRecordRepository;
import com.vz.backend.business.service.AttachmentService;
import com.vz.backend.business.service.DocumentOutAttachmentService;
import com.vz.backend.business.service.NotificationService;
import com.vz.backend.business.service.TaskAttachmentService;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Constant;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.config.ModuleCodeEnum;
import com.vz.backend.core.config.NotificationHandleStatusEnum;
import com.vz.backend.core.domain.Organization;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.exception.RestResponse;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;
import com.vz.backend.core.service.OrganizationService;
import com.vz.backend.util.StringUtils;

@Service
public class HsFolderRecordFormService extends BaseService<HsFolderRecordForm> {

	@Value("${ecm.domain}")
	private String ecmDomain;

	@Value("${ecm.form.register}")
	private String ecmRegisterFormUrl;

	@Value("${ecm.folder.register}")
	private String ecmRegisterFolderUrl;

	@Autowired
	private IHsFolderRecordFormRepository formRepository;

	@Autowired
	private IHsFolderRecordRepository folderRecordRepository;

	@Autowired
	private OrganizationService orgService;

	@Autowired
	private TaskAttachmentService taskAtmService;

	@Autowired
	private NotificationService notificationService;

	@Autowired
	private HsFolderService folderService;

	@Autowired
	private HsFolderFileService folderFileService;

	@Autowired
	private AttachmentService docInAtmService;

	@Autowired
	private DocumentOutAttachmentService docOutAtmService;

	@Override
	public IRepository<HsFolderRecordForm> getRepository() {
		return formRepository;
	}

	private final String ecmErrorMessage = "Lỗi kết nối tới ECM";

	/**
	 * For testing
	 * @param object
	 */
	private void convertJsonObjectToString(Object object) {
		ObjectWriter ow = new ObjectMapper().writer().withDefaultPrettyPrinter();
		try {
			String json = ow.writeValueAsString(object);
		} catch (JsonProcessingException e1) {
			e1.printStackTrace();
		}
	}

	/**
	 * Register form to ECM
	 * @param input
	 * @param isDelTemp
	 * @return
	 */
	public Object register(HsFolderRecordForm input, boolean isDelTemp) {
		HsFolderRecordFormDto dto = convert(input);
		convertJsonObjectToString(dto);
		RestTemplate restTemplate = new RestTemplate();
		Object object = null;
		try {
			input.setStatus(2);
			formRepository.save(input);
			object = restTemplate.postForObject(ecmDomain + ecmRegisterFormUrl, dto, Object.class);
		} catch (HttpClientErrorException e1) {
			throw new RestExceptionHandler(BussinessCommon.getMessage(e1, ecmErrorMessage));
		} catch (Exception e) {
			e.printStackTrace();
			if (isDelTemp) {
				formRepository.delete(input);
			}
			throw new RestExceptionHandler(ecmErrorMessage);
		}
		return object;
	}

	/**
	 * Convert form register to dto
	 * @param input
	 * @return
	 */
	private HsFolderRecordFormDto convert(HsFolderRecordForm input) {
		HsFolderRecordFormDto dto = new HsFolderRecordFormDto();
		dto.convert(input, false);
		List<TaskAttachment> atms = taskAtmService.findByObjId(input.getId(), Constant.TYPE_FOLDER_FORM,
				BussinessCommon.getClientId(), true);
		atms.forEach(i -> dto.getDinhKem().add(new FolderAttachmentDto(i)));
		return dto;
	}

	/**
	 * Get register form detail
	 * @param formId
	 * @return
	 */
	public HsFolderRecordForm getDetail(Long formId) {
		HsFolderRecordForm input = valid(formId, Message.NOT_FOUND_OBJECT);
		List<TaskAttachment> atms = taskAtmService.findByObjId(input.getId(), Constant.TYPE_FOLDER_FORM,
				BussinessCommon.getClientId(), true);
		input.setAttachments(atms);
		return input;
	}

	/**
	 * Save register form
	 */
	public HsFolderRecordForm save(HsFolderRecordForm input) {
		input.valids();
		User user = BussinessCommon.getUser();
		input.setOrgId(user.getOrg());
		if (Constant.PHONG.equalsIgnoreCase(user.getOrgModel().getOrgTypeModel().getName())) {
			Organization org = orgService.findParentOrgByTypeAndOrgId(Constant.CUC_VU_VIEN, user.getOrgModel());
			if (org == null)
				throw new RestExceptionHandler("Cấp tổ chức không hợp lệ");
			input.setOrgId(org.getId());
		}

		existName(input.getName());
		return formRepository.save(input);
	}

	private void existName(String name) {
		long count = formRepository.countByName(name, BussinessCommon.getClientId());
		if (count > 0)
			throw new RestExceptionHandler("Tên phiếu đã tồn tại");
	}

	/**
	 * Update register form
	 * @param input
	 * @param id
	 * @return
	 */
	public HsFolderRecordForm update(HsFolderRecordForm input, Long id) {
		HsFolderRecordForm form = valid(id, Message.NOT_FOUND_OBJECT);
		input.valids();
		if (!input.getName().equals(form.getName())) {
			existName(input.getName());
		}

		form.set(input);
		return formRepository.save(input);
	}

	/**
	 * Export register form data
	 * @param outputStream
	 * @param f
	 */
	public void export(OutputStream outputStream, HsFolderRecordForm f) {
		f.valids();
		ExportFolderRecordForm dto = new ExportFolderRecordForm();
		dto.convert(f, true);
		try {
			DocxStamperConfiguration stamperConfig = new DocxStamperConfiguration();
			stamperConfig.setLineBreakPlaceholder(Calendar2Part.BREAK_LINE);
			@SuppressWarnings("unchecked")
			DocxStamper<ExportFolderRecordForm> stamper = stamperConfig.build();
			File fs = ResourceUtils.getFile("classpath:templates/Đăng ký nộp lưu.docx");
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

	/**
	 * Get status list
	 * @param register
	 * @return
	 */
	private List<Integer> getStatus(Boolean register) {
		List<Integer> status = new ArrayList<>();
		if (Boolean.TRUE.equals(register)) {
			status.add(2);
			status.add(3);
			status.add(4);
			status.add(5);
		} else {
			status.add(1);
		}
		return status;
	}

	/**
	 * Get list form register
	 * @param register
	 * @param page
	 * @return
	 */
	public Page<HsFolderRecordFormListDto> list(Boolean register, Integer page) {
		Pageable pageable = BussinessCommon.castToPageable(page);
		List<Integer> status = getStatus(register);
		return formRepository.list(status, BussinessCommon.getClientId(), pageable);
	}

	/**
	 * Valid folder id list
	 * @param ids
	 * @return
	 */
	private List<HsFolder> validFolderId(List<Long> ids) {
		if (BussinessCommon.isEmptyList(ids))
			throw new RestExceptionHandler(Message.WRONG_INPUT_DATA);
		List<HsFolder> folders = folderService.getByIds(ids);
		if (ids.size() != folders.size()) {
			throw new RestExceptionHandler(Message.WRONG_INPUT_DATA);
		}
		return folders;
	}

	private List<HsFolder> folderTmps = null;

	/**
	 * Save folder records list by id
	 * @param formId
	 * @param ids    : folders record id
	 * @return
	 */
	public List<HsFolderRecord> saveFolderRecords(Long formId, List<Long> ids) {
		valid(formId, Message.NOT_FOUND_OBJECT);
		List<HsFolder> folders = validFolderId(ids);
		folderTmps = folders;
		List<HsFolderRecord> rs = new ArrayList<>();
		for (HsFolder h : folders) {
			rs.add(new HsFolderRecord(h.getId(), formId));
		}
		return folderRecordRepository.saveAll(rs);
	}

	/**
	 * Register folder and formId to ECM
	 * @param formId
	 * @param input
	 * @return
	 */
	public Object register(Long formId, List<HsFolderRecord> input) {
		HsFolderRecordEcmDto dto = new HsFolderRecordEcmDto(formId);
		dto.setData(convert(input));
		convertJsonObjectToString(dto);
		RestTemplate restTemplate = new RestTemplate();
		Object object = null;
		try {
			object = restTemplate.postForObject(ecmDomain + ecmRegisterFolderUrl, dto, Object.class);
		} catch (HttpClientErrorException e1) {
			throw new RestExceptionHandler(BussinessCommon.getMessage(e1, ecmErrorMessage));
		} catch (Exception e) {
			e.printStackTrace();
			throw new RestExceptionHandler(ecmErrorMessage);
		}
		return object;
	}

	/**
	 * Get folder map
	 * @param folders
	 * @return
	 */
	private Map<Long, HsFolder> toMap(List<HsFolder> folders) {
		Map<Long, HsFolder> tmp = new HashMap<>();
		folders.forEach(i -> tmp.put(i.getId(), i));
		return tmp;
	}

	/**
	 * Convert data to dto
	 * @param input
	 * @return
	 */
	private List<HsFolderRecordDto> convert(List<HsFolderRecord> input) {
		List<HsFolderRecordDto> rs = new ArrayList<>();
		try {

			// get folders list
			List<Long> folderIds = input.stream().map(HsFolderRecord::getHsFolderId).collect(Collectors.toList());
			List<HsFolder> folders = folderTmps == null ? folderService.getByIds(folderIds) : folderTmps;

			// get attachment by folders
			Map<Long, List<FolderAttachmentDto>> fileMap = getByFolderIds(folderIds);

			// valid files
			validFileByFolderIds(folders, fileMap);

			// get folder map
			Map<Long, HsFolder> folderMap = toMap(folders);
			for (HsFolderRecord i : input) {
				i.setStatus(2);
				folderRecordRepository.save(i);
				Long key = i.getHsFolderId();
				if (folderMap.containsKey(key)) {
					i.setHsFolder(folderMap.get(key));
				}

				if (fileMap.containsKey(key)) {
					rs.add(new HsFolderRecordDto(i, fileMap.get(key)));
				}
			}
		} catch (RestExceptionHandler e) {
			folderRecordRepository.deleteAll(input);
			throw e;
		} catch (Exception e) {
			e.printStackTrace();
			folderRecordRepository.deleteAll(input);
			throw new RestExceptionHandler(Message.ERROR_SYS);
		}
		return rs;
	}

	/**
	 * Get list folder register
	 * @param register
	 * @param page
	 * @return
	 */
	public Page<HsFolderRecordListDto> getListFolderRecord(Boolean register, Integer page) {
		Pageable pageable = BussinessCommon.castToPageable(page);

		if (Boolean.TRUE.equals(register)) {
			List<Integer> status = getStatus(register);
			return folderRecordRepository.list(status, BussinessCommon.getClientId(), pageable);
		} else {
			return folderService.listApprove(page);
		}
	}

	/**
	 * Valid status
	 * @param status
	 */
	private void validStatus(Integer status) {
		if (status == null || status < 1 || status > 5) {
			throw new RestExceptionHandler("Dữ liệu trường trạng thái không hợp lệ");
		}
	}

	/**
	 * Valid folder records id
	 * @param ids
	 * @return
	 */
	private List<HsFolderRecord> validByFolderRecordIds(List<Long> ids) {
		List<HsFolderRecord> rs = folderRecordRepository
				.listfindByActiveAndId(ids);

		if (rs.size() != ids.size())
			throw new RestExceptionHandler(Message.WRONG_INPUT_DATA);

		return rs;
	}

	/**
	 * Update folder record status
	 * @param formId
	 * @param dto
	 * @return
	 */
	public List<HsFolderRecord> updateFolderRecords(Long formId, FolderStatusListDto dto) {
		List<FolderStatusDto> data = dto.getData();

		// valid data
		if (BussinessCommon.isEmptyList(data))
			throw new RestExceptionHandler(Message.WRONG_INPUT_DATA);

		BussinessCommon.validLengthData(dto.getLyDo(), "Lý do phản hồi", 250);

		List<Integer> statusInputs = data.stream().map(FolderStatusDto::getStatus).collect(Collectors.toList());
		statusInputs.forEach(i -> validStatus(i));

		HsFolderRecordForm form = formRepository.findByActiveAndId(formId);

		boolean reject = false;
		HashMap<Long, Integer> tmp = new HashMap<>();
		data.forEach(i -> {
			if (i.getId() != null && i.getStatus() != null) {
				tmp.put(i.getId(), i.getStatus());
			}
		});

		List<Long> ids = data.stream().map(FolderStatusDto::getId).collect(Collectors.toList());
		List<HsFolderRecord> rs = validByFolderRecordIds(ids);

		// set status
		for (HsFolderRecord f : rs) {
			Long key = f.getId();
			if (tmp.containsKey(key)) {
				f.setStatus(tmp.get(key));
			}

			if (!tmp.get(key).equals(Constant.HS_APPROVE)) {
				reject = true;
			}
		}

		// update form
		form.setDateResponse(dto.getNgayXuLy());
		form.setUserResponse(dto.getUserNameXuLY());
		form.setNoteResponse(dto.getLyDo());
		formRepository.save(form);

		// inform create by
		NotificationHandleStatusEnum notiStatus = !reject ? NotificationHandleStatusEnum.NOP_LUU_DA_DUYET
				: NotificationHandleStatusEnum.NOP_LUU_TU_CHOI;
		notificationService.add(form.getCreateBy(), formId, form.getSrc().name, DocumentTypeEnum.TU_HO_SO, notiStatus,
				ModuleCodeEnum.HSTL_NOPLUU);
		return folderRecordRepository.saveAll(rs);
	}

	/**
	 * Get attachments by folders id
	 * @param ids: folders id
	 */
	private HashMap<Long, List<FolderAttachmentDto>> getByFolderIds(List<Long> ids) {
		HashMap<Long, List<FolderAttachmentDto>> rs = new HashMap<>();
		for (Long i : ids) {
			rs.put(i, new ArrayList<>());
		}

		// folder file
		List<FolderAttachmentDto> folderFiles = folderFileService.getByFolderIds(ids);
		folderFiles.forEach(i -> {
			Long key = i.getFolderId();
			List<FolderAttachmentDto> value = new ArrayList<>();
			if (rs.containsKey(key)) {
				value = rs.get(key);
			}
			value.add(i);
			rs.put(key, value);
		});

		// document in file
		List<FolderAttachmentDto> docInAtms = docInAtmService.getByFolderIds(ids);
		docInAtms.forEach(i -> {
			Long key = i.getFolderId();
			List<FolderAttachmentDto> value = new ArrayList<>();
			if (rs.containsKey(key)) {
				value = rs.get(key);
			}
			value.add(i);
			rs.put(key, value);
		});

		// document out file
		List<FolderAttachmentDto> docOutAtms = docOutAtmService.getByFolderIds(ids);
		docOutAtms.forEach(i -> {
			Long key = i.getFolderId();
			List<FolderAttachmentDto> value = new ArrayList<>();
			if (rs.containsKey(key)) {
				value = rs.get(key);
			}
			value.add(i);
			rs.put(key, value);
		});

		return rs;
	}

	/**
	 * Valid files by folder id
	 */
	private void validFileByFolderIds(List<HsFolder> folders, Map<Long, List<FolderAttachmentDto>> map) {
		HashMap<Long, String> nameMaps = new HashMap<>();
		folders.forEach(i -> nameMaps.put(i.getId(), i.getTitle()));

		StringBuilder tmp = new StringBuilder();
		map.forEach((k, v) -> {
			if (v.isEmpty() && nameMaps.containsKey(k)) {
				if (tmp.length() > 0) {
					tmp.append(", ");
				}
				tmp.append(nameMaps.get(k));
			}
		});

		if (tmp.length() > 0) {
			throw new RestExceptionHandler(tmp.toString() + " chưa có tệp để đăng kí nộp lưu");
		}
	}

	public List<FormRegisterDto> all() {
		return formRepository.all(BussinessCommon.getClientId());
	}

	/**
	 * Delete form
	 */
	public Boolean delForm(Long formId) {
		HsFolderRecordForm form = valid(formId, Message.NOT_FOUND_OBJECT);
		form.setActive(false);
		formRepository.save(form);
		return true;
	}

	/**
	 * Update status of form register
	 */
	public HsFolderRecordForm updateStatusRegisterForm(Long id, UpdateStatusRegisterFormDto dto) {
		// valid info
		HsFolderRecordForm form = formRepository.findByActiveAndId(id);
		Integer status = dto.getStatus();
		validStatus(status);
		BussinessCommon.validLengthData(dto.getLyDo(), "Lý do phản hồi", 250);

		// save info
		form.setStatus(status);
		form.setDateResponse(dto.getNgayXuLy());
		form.setUserResponse(dto.getUserNameXuLY());
		form.setNoteResponse(dto.getLyDo());
		formRepository.save(form);

		// inform create by
		NotificationHandleStatusEnum notiStatus = status == 5 ? NotificationHandleStatusEnum.NOP_LUU_DA_DUYET
				: NotificationHandleStatusEnum.NOP_LUU_TU_CHOI;
		notificationService.add(form.getCreateBy(), id, form.getSrc().name, DocumentTypeEnum.TU_HO_SO, notiStatus,
				ModuleCodeEnum.HSTL_NOPLUU);
		return form;
	}

	/**
	 * Save file from OTHERS SYSTEM
	 */
	public Boolean savefile() {
		String domain = "http://172.16.10.80:8080/api/hstl-form/download/1/Danh mục hồ sơ (6).doc__1634022937075";
		// Option 1: using httpURLConnection
		URL url = null;
		HttpURLConnection httpConn = null;
		try {
			domain = StringUtils.decodeFromUrl(domain);
			url = new URL(domain);
			httpConn = (HttpURLConnection) url.openConnection();
			httpConn.setRequestMethod("GET");
			httpConn.setRequestProperty("User-Agent",
					"Mozilla/5.0 (Windows; U; Windows NT 6.0; en-US; rv:1.9.0.5) Gecko/2008120122 Firefox/3.0.5");
			httpConn.setRequestProperty("Accept", "*/*");
			httpConn.setDoOutput(true);
			int responseCode = httpConn.getResponseCode();
			if (responseCode == HttpURLConnection.HTTP_OK) {
				return true;
			}
		} catch (Exception e) {
			e.printStackTrace();
			return false;
		} finally {
			if (httpConn != null)
				httpConn.disconnect();
		}

		// Option 2: using rest template
		RestTemplate restTemplate = new RestTemplate();
		byte[] data = restTemplate.getForObject(domain, byte[].class);

		File tmp = new File("abc.doc");
		try {
			if (tmp.createNewFile()) {
				OutputStream os = new FileOutputStream(tmp);
				os.write(data);
				os.close();
			}
		} catch (Exception e) {
			e.printStackTrace();
		}

		if (tmp.exists()) {
			tmp.delete();
		}
		return true;
	}
}
