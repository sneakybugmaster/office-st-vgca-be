package com.vz.backend.business.service;

import java.io.ByteArrayInputStream;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.commons.lang.ArrayUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.StringUtils;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;

import com.vz.backend.business.domain.DocumentOut;
import com.vz.backend.business.domain.DocumentOutAttachment;
import com.vz.backend.business.domain.Documents;
import com.vz.backend.business.domain.outsideconnect.TrackingConnectOutside;
import com.vz.backend.business.domain.outsideconnect.TrackingObjectOutside;
import com.vz.backend.business.dto.outsideconnect.Attachment;
import com.vz.backend.business.dto.outsideconnect.Content;
import com.vz.backend.business.dto.outsideconnect.DataRawRequestOutside;
import com.vz.backend.business.dto.outsideconnect.DataRequestOutside;
import com.vz.backend.business.dto.outsideconnect.ErrorMsg;
import com.vz.backend.business.dto.outsideconnect.OutsideSystemDto;
import com.vz.backend.business.dto.outsideconnect.OutsideSystemDto.TYPE;
import com.vz.backend.business.dto.outsideconnect.RejectDataDto;
import com.vz.backend.business.dto.outsideconnect.ResultMsg;
import com.vz.backend.business.dto.outsideconnect.SentDataDto;
import com.vz.backend.business.service.outsideconnect.TrackingConnectOutsideService;
import com.vz.backend.business.service.outsideconnect.TrackingObjectOutsideService;
import com.vz.backend.core.auth.TokenHelper;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.ActionConnectEnum;
import com.vz.backend.core.config.Constant;
import com.vz.backend.core.config.DocumentInTrackingEnum;
import com.vz.backend.core.config.DocumentOutTrackingEnum;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.config.ModuleCodeEnum;
import com.vz.backend.core.config.NotificationHandleStatusEnum;
import com.vz.backend.core.domain.Category;
import com.vz.backend.core.domain.Encryption;
import com.vz.backend.core.domain.OutsideSystem;
import com.vz.backend.core.domain.Token;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.dto.IdName;
import com.vz.backend.core.dto.LabelValueId;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.exception.RestResponse;
import com.vz.backend.core.service.CategoryService;
import com.vz.backend.core.service.EncryptionService;
import com.vz.backend.core.service.FilesStorageService;
import com.vz.backend.core.service.OrganizationService;
import com.vz.backend.core.service.OutsideSystemService;
import com.vz.backend.core.service.UserService;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class OutsideConnectService {

	private final Path root = Paths.get("uploads");
	private final Path enc = Paths.get(Constant.ENCRYPT_FILE_PATH);

	@Value("${configs.clerical-org: false}")
	private boolean clericalOrg;

	@Autowired
	private OutsideSystemService outsideSystemService;

	@Autowired
	private TokenHelper tokenProvider;

	@Autowired
	private UserService userService;

	@Autowired
	private DocumentOutService outService;

	@Autowired
	private DocumentService inService;

	@Autowired
	private DocumentOutAttachmentService outAtmService;

	@Autowired
	private CategoryService catService;

	@Autowired
	private OrganizationService orgService;

	@Autowired
	private ClericalOrgService clericalOrgService;

	@Autowired
	private TrackingConnectOutsideService trackingService;

	@Autowired
	private TrackingObjectOutsideService trackingObjService;

	@Autowired
	private NotificationService noticeService;

	@Autowired
	private DocumentInTrackingService trackingServices;

	@Autowired
	private FilesStorageService storageService;

	@Autowired
	private AttachmentService inAtmService;

	@Autowired
	private EncryptionService encryptionService;

	@Autowired
	private CommonService commonService;

	@Autowired
	private NotificationService notiService;

	@Autowired
	private DocumentOutTrackingService outTrackService;;

	private String msgDf = "Lỗi kết nối tới hệ thống ngoài : ";

	/**
	 * Send request connect to outside system
	 * 
	 * @param outsideId
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public Boolean requestConnect(OutsideSystem input) {
		input.valids();
		input.setFrDomain(BussinessCommon.getDomain());
		RestTemplate restTemplate = new RestTemplate();
		String url = getUrlByCode(input.getDomain(), "connect");
		OutsideSystem old = outsideSystemService.findByDomainAndKey(input.getDomain(), input.getKey());
		try {
			RestResponse rs = restTemplate.postForObject(url, input, RestResponse.class);
			LinkedHashMap<String, Object> data = (LinkedHashMap<String, Object>) rs.getData();
			if (data != null) {
				if (old == null) {
					old = input;
				}
				old.setToken(String.valueOf(data.get("accessToken")));
				old.setTimeExpired(new Date((long) data.get("timeExprise")));
				old.setActive(true);
				outsideSystemService.save(old);

				// Tracking connect
				trackingService.save(new TrackingConnectOutside(old, ActionConnectEnum.CONNECT, true));
				return true;
			}
		} catch (HttpClientErrorException e) {
			throw new RestExceptionHandler(BussinessCommon.getMessage(e, msgDf + input.getName()));
		} catch (Exception e) {
			throw new RestExceptionHandler(msgDf + input.getName());
		}

		// Tracking connect
		trackingService.save(new TrackingConnectOutside(input, ActionConnectEnum.CONNECT, false));
		return false;
	}

	private String getUrlByCode(String domain, String code) {
		if (StringUtils.isEmpty(domain))
			return null;
		StringBuffer rs = new StringBuffer(domain);
		switch (code) {
		case "connect":
			rs.append("/api/integrate/rs-connect");
			break;
		case "org":
			rs.append("/api/integrate/rs-org");
			break;
		case "obj":
			rs.append("/api/integrate/receive-obj");
			break;
		case "atm":
			rs.append("/api/integrate/download");
			break;
		case "cert":
			rs.append("/api/integrate/cert/rs-user");
			break;
		case "encrypt":
			rs.append("/api/integrate/encrypt/rs-add");
			break;
		case "reject-obj":
			rs.append("/api/integrate/obj/rs-reject");
			break;
		default:
			break;
		}
		return rs.toString();
	}

	private HttpEntity<String> getHeaders(String token) {
		if (StringUtils.isEmpty(token)) {
			return null;
		}

		HttpHeaders headers = new HttpHeaders();
		headers.add("Authorization", "Bearer " + token);
		HttpEntity<String> entity = new HttpEntity<String>(headers);
		return entity;
	}

	private HttpEntity<Object> getHeaders(String token, Object object) {
		if (StringUtils.isEmpty(token)) {
			return null;
		}

		HttpHeaders headers = new HttpHeaders();
		headers.add("Authorization", "Bearer " + token);
		HttpEntity<Object> request = new HttpEntity<>(object, headers);
		return request;
	}

	/**
	 * Handle request connect from outside
	 * 
	 * @param data
	 * @return
	 */
	public Token responseConnect(OutsideSystem data) {
		data.valids();
		OutsideSystem old = outsideSystemService.findByDomainAndKey(data.getFrDomain(), data.getKey());
		User user = userService.addUserViaConnectOutside();
		if (old == null) {
			// Tracking connect
			trackingService.save(new TrackingConnectOutside(old, ActionConnectEnum.CONNECT, false));
			throw new RestExceptionHandler(Message.NOT_FOUND_CONNECT_SYSTEM);
		}

		Token tk = tokenProvider.generateToken(user.getUserName());
		// Tracking connect
		trackingService.save(new TrackingConnectOutside(old, ActionConnectEnum.CONNECT, true));
//		old.setToken(tk.getAccessToken());
//		old.setTimeExpired(tk.getTimeExprise());
//		outsideSystemService.save(old);
		return tk;
	}

	/**
	 * Get data organization that register connective outside
	 * 
	 * @param outsideId
	 * @return
	 */
	@SuppressWarnings({ "rawtypes", "unchecked" })
	public List<LinkedHashMap<String, String>> getAllByGlobal(Long outsideId) {
		OutsideSystem input = outsideSystemService.valid(outsideId, Message.NOT_FOUND_CONNECT_SYSTEM);
		input.setFrDomain(BussinessCommon.getDomain());
		RestTemplate restTemplate = new RestTemplate();
		String url = getUrlByCode(input.getDomain(), "org");
		try {
			HttpEntity headers = getHeaders(input.getToken(), input);
			ResponseEntity<RestResponse> response = restTemplate.exchange(url, HttpMethod.POST, headers,
					RestResponse.class);
			List<LinkedHashMap<String, String>> rp = (List<LinkedHashMap<String, String>>) response.getBody().getData();
			if (rp != null) {
				rp.forEach(i -> {
					String tmp = (String) i.get("orgName");
					trackingService.save(new TrackingConnectOutside(input, ActionConnectEnum.REQUEST_ORG, tmp, true));
				});
			}

			return rp;
		} catch (HttpClientErrorException e) {
			trackingService.save(new TrackingConnectOutside(input, ActionConnectEnum.REQUEST_ORG, false));
			throw new RestExceptionHandler(BussinessCommon.getMessage(e, msgDf + input.getName()));
		} catch (Exception e) {
			e.printStackTrace();
			trackingService.save(new TrackingConnectOutside(input, ActionConnectEnum.REQUEST_ORG, false));
			throw new RestExceptionHandler(msgDf + input.getName());
		}
	}

	/**
	 * Handle send request list system outside
	 * 
	 * @param data
	 * @return
	 */
	public ResultMsg sendObjList(DataRawRequestOutside data) {
		List<DataRequestOutside> rqs = new ArrayList<>();
		data.valids();
		Content content = validContent(data.getObjId(), data.getType());
		data.getOutsideIds().forEach(i -> {
			OutsideSystem tmp = outsideSystemService.valid(i.getOutsideId(), Message.NOT_FOUND_CONNECT_SYSTEM);
			i.set(tmp, TYPE.ORG);
			rqs.add(new DataRequestOutside(i, content));
		});

		List<String> successSystem = new ArrayList<>();
		List<ErrorMsg> failSystem = new ArrayList<>();
		for (DataRequestOutside j : rqs) {
			if (sendObj(j)) {
				successSystem.add(j.getSys().getName());
			} else {
				failSystem.add(new ErrorMsg(j.getSys().getName(), errors));
			}
		}
		return new ResultMsg(successSystem, failSystem);
	}

	/**
	 * Handle send request each system outside
	 * 
	 * @param rq
	 */
	private List<String> errors = new ArrayList<>();

	@SuppressWarnings({ "rawtypes" })
	private boolean sendObj(DataRequestOutside rq) {
		RestTemplate restTemplate = new RestTemplate();
		String url = getUrlByCode(rq.getSys().getDomain(), "obj");
		log.info("Sending object to " + rq.getSys().getName() + " with domain : " + rq.getSys().getDomain() + "...");
		errors = new ArrayList<>();
		String err = null;
		try {
			HttpEntity headers = getHeaders(rq.getSys().getToken(), rq);
			ResponseEntity<RestResponse> response = restTemplate.exchange(url, HttpMethod.POST, headers,
					RestResponse.class);
			HttpStatus status = response.getStatusCode();
			if (status == HttpStatus.OK) {
				// Add tracking document
				trackingService.save(new TrackingConnectOutside(rq.getOutSideSystem(), ActionConnectEnum.SEND_DOC,
						rq.getContent().getPreview(), true));
				trackingObjService.saveAll(new TrackingObjectOutside().convert(rq.getSys(), rq.getContent().getObjId(),
						DocumentTypeEnum.VAN_BAN_DI, true));
				return true;
			}

			log.error("Error connect object to out systems : " + status.value());
			// Add tracking document
			trackingService.save(new TrackingConnectOutside(rq.getOutSideSystem(), ActionConnectEnum.SEND_DOC,
					rq.getContent().getPreview(), false));
			trackingObjService.saveAll(new TrackingObjectOutside().convert(rq.getSys(), rq.getContent().getObjId(),
					DocumentTypeEnum.VAN_BAN_DI, false));
			err = msgDf + rq.getSys().getName();
			errors.add(err);
		} catch (HttpClientErrorException e) {
//			throw new RestExceptionHandler(BussinessCommon.getMessage(e, msgDf + rq.getSys().getName()));
			err = BussinessCommon.getMessage(e, msgDf + rq.getSys().getName());
			errors.add(err);
			log.error(err);
		} catch (Exception e) {
//			throw new RestExceptionHandler(msgDf + rq.getSys().getName());
			err = msgDf + rq.getSys().getName();
			errors.add(err);
			log.error(err);
		}
		return false;
	}

	/**
	 * Convert document & attachments before send to outside
	 * 
	 * @param objId
	 * @param type
	 * @return
	 */
	private Content validContent(Long objId, DocumentTypeEnum type) {
		switch (type) {
		case VAN_BAN_DI:
			DocumentOut out = outService.valid(objId, Message.NOT_FOUND_DOC_OUT);
			List<DocumentOutAttachment> outAtms = outAtmService.getListAttachment(objId);
			return new Content(out, outAtms);
		default:
			break;
		}
		throw new RestExceptionHandler(Message.NOT_FOUND_OBJECT);
	}

	/**
	 * Handle receive object from outside
	 * 
	 * @param data
	 * @return
	 */
	@Transactional
	public Object receiveObj(DataRequestOutside data) {
		// Check connect
		data.valids();
		OutsideSystemDto sysInfo = data.getSys();
		OutsideSystem old = outsideSystemService.findByDomainAndKey(sysInfo.getFrDomain(), sysInfo.getKey());
		if (old == null) {
			// Add tracking document
			trackingService.save(new TrackingConnectOutside(old, ActionConnectEnum.RECEIVE_DOC,
					data.getContent().getPreview(), false));
			throw new RestExceptionHandler(Message.NOT_FOUND_CONNECT_SYSTEM);
		}

		try {
			// Valid data request
			Map<Long, List<Long>> userIds = validOrgOutside(sysInfo.getOrgs());
			for (IdName i : sysInfo.getOrgs()) {
				// Handle document
				Documents doc = handleDocument(data.getContent(), i.getId(), i.getName());

				// Handle attachment
				saveAtm(doc, sysInfo, data.getContent().getAtms());

				// Inform person receive document
				List<Long> tmp = userIds.get(i.getId());
				noticeService.addAll(tmp, doc.getId(), "VB liên kết : " + doc.getPreview(),
						DocumentTypeEnum.VAN_BAN_DEN, NotificationHandleStatusEnum.CHO_TIEP_NHAN,
						ModuleCodeEnum.DOC_OUT_LIST);
				trackingServices.saveAll(doc.getId(), DocumentInTrackingEnum.RECEIVE,
						data.getContent().getDocTypeName(), tmp);

				// Add tracking document
				trackingService.save(new TrackingConnectOutside(old, ActionConnectEnum.RECEIVE_DOC,
						data.getContent().getPreview(), true));
				trackingObjService.save(new TrackingObjectOutside(old.getId(), i.getId(), i.getName(), doc.getId(),
						DocumentTypeEnum.VAN_BAN_DI, true, data.getContent().getObjId()));
			}
		} catch (RestExceptionHandler e) {
			trackingService.save(new TrackingConnectOutside(old, ActionConnectEnum.RECEIVE_DOC,
					data.getContent().getPreview(), false));
			throw e;
		} catch (Exception e) {
			trackingService.save(new TrackingConnectOutside(old, ActionConnectEnum.RECEIVE_DOC,
					data.getContent().getPreview(), false));
			e.printStackTrace();
			throw new RestExceptionHandler(msgDf + old.getName());
		}
		return true;
	}

	/**
	 * Save files in organization receive
	 * 
	 * @param doc
	 * @param sysInfo
	 * @param atms
	 */
	private void saveAtm(Documents doc, OutsideSystemDto sysInfo, List<Attachment> atms) {
		if (BussinessCommon.isEmptyList(atms))
			return;

		// Success atms saved
		List<String> scNormalFileNames = new ArrayList<>();
		List<String> scEncFileNames = new ArrayList<>();
		for (Attachment i : atms) {
			try {
				saveFile(sysInfo.getFrDomain(), sysInfo.getToken(), i.getName(), i.getEncrypt());
				if (Boolean.TRUE.equals(i.getEncrypt())) {
					scEncFileNames.add(i.getName());
				} else {
					scNormalFileNames.add(i.getName());
				}
			} catch (Exception e) { // rollback
				inAtmService.deleteAllByDocId(doc.getId());
				storageService.delete(root, scNormalFileNames);
				encryptionService.delEncrypt(scEncFileNames);
				inService.deleteDoc(doc);
				throw new RestExceptionHandler("Tệp được tải không thành công : " + i.getDisplayName()
						+ " từ hệ thống : " + sysInfo.getName());
			}
		}
	}

	/**
	 * Save file in organization receive
	 * 
	 * @param domain
	 * @param token
	 * @param fileName (parsed)
	 */
	@SuppressWarnings("rawtypes")
	private void saveFile(String domain, String token, String fileName, Boolean encrypt) {
		RestTemplate restTemplate = new RestTemplate();
		String url = getUrlByCode(domain, "atm") + "/" + fileName;
		log.info("Downloading file with " + url);
		HttpEntity headers = getHeaders(token);
		ResponseEntity<byte[]> response = restTemplate.exchange(url, HttpMethod.GET, headers, byte[].class);
		Path tmpPath = Boolean.TRUE.equals(encrypt) ? enc : root;
		storageService.save(new ByteArrayInputStream(response.getBody()), fileName, tmpPath);
	}

	/**
	 * Valid organization request when receive data from outside
	 * 
	 * @param opt
	 * @param data
	 */
	private Map<Long, List<Long>> validOrgOutside(List<IdName> orgs) {
		Map<Long, List<Long>> userIds = new HashMap<>();
		List<Long> orgIds = orgs.stream().map(IdName::getId).collect(Collectors.toList());

		// Check data valid
		orgService.validOrgIdsByOutside(orgIds);

		// Check data has clerical
		orgs.forEach(i -> {
			List<Long> tmp = clericalOrg ? clericalOrgService.getClericalOrgByOrgId(i.getId())
					: userService.getListIdsVanThuVBDenByOrg(i.getId());
			if (tmp.isEmpty()) {
				throw new RestExceptionHandler("Không tìm thấy văn thư đơn vị của tổ chức " + i.getName());
			}
			userIds.put(i.getId(), tmp);
		});

		return userIds;
	}

	/**
	 * Save document from outside
	 * 
	 * @param objInfo
	 * @param orgId     : organization that receive this document
	 * @param orgSender : organization that sent this document
	 * @return
	 */
	private Documents handleDocument(Content objInfo, Long orgId, String orgSender) {
		Category security = catService.findByNameAndCategoryCode(objInfo.getSecurityName(),
				com.vz.backend.core.config.Constant.CAT_SECURE);
		if (security == null) {
			security = catService.save(objInfo.getSecurityName(), com.vz.backend.core.config.Constant.CAT_SECURE,
					"Độ mật");
		}

		Category urgent = catService.findByNameAndCategoryCode(objInfo.getUrgentName(),
				com.vz.backend.core.config.Constant.CAT_URENT);
		if (urgent == null) {
			urgent = catService.save(objInfo.getUrgentName(), com.vz.backend.core.config.Constant.CAT_URENT, "Độ khẩn");
		}

		Category fields = catService.findByNameAndCategoryCode(objInfo.getDocFieldName(),
				com.vz.backend.core.config.Constant.CAT_FIELD);
		if (fields == null) {
			fields = catService.save(objInfo.getDocFieldName(), com.vz.backend.core.config.Constant.CAT_FIELD,
					"Lĩnh vực");
		}

		Category type = catService.findByNameAndCategoryCode(objInfo.getDocTypeName(),
				com.vz.backend.core.config.Constant.CAT_DOC_TYPE);
		if (type == null) {
			type = catService.save(objInfo.getDocTypeName(), com.vz.backend.core.config.Constant.CAT_DOC_TYPE,
					"Loại văn bản");
		}

		Category receive = catService.findByNameAndCategoryCode("Điện tử",
				com.vz.backend.core.config.Constant.CAT_TYPE_RECEIVE);
		if (receive == null) {
			receive = catService.save(objInfo.getDocTypeName(), com.vz.backend.core.config.Constant.CAT_TYPE_RECEIVE,
					"Phương thức nhận văn bản");
		}

		Category placeSend = catService.findByNameAndCategoryCode(orgSender, Constant.CAT_ORG_OTHER);
		if (placeSend == null) {
			placeSend = catService.save(orgSender, Constant.CAT_ORG_OTHER, "Nơi nhận bên ngoài");
		}

		Documents doc = new Documents(objInfo);
		doc.setDocFieldsId(fields.getId());
		doc.setDocTypeId(type.getId());
		doc.setUrgentId(urgent.getId());
		doc.setSecurityId(security.getId());
		doc.setOrgReceiveId(orgId);
		doc.setPlaceSendId(placeSend.getId());
		doc.setMethodReceiptId(receive.getId());
		return inService.save(doc);
	}

	/**
	 * Get organization that configure to connect outside
	 * 
	 * @param data
	 * @return
	 */
	public List<SentDataDto> getAllByGlobal(OutsideSystem data) {
		data.valids();
		OutsideSystem old = outsideSystemService.findByDomainAndKey(data.getFrDomain(), data.getKey());
		if (old == null) {
			// Tracking connect
			trackingService.save(new TrackingConnectOutside(old, ActionConnectEnum.REQUEST_ORG, false));
			throw new RestExceptionHandler(Message.NOT_FOUND_CONNECT_SYSTEM);
		}

		List<IdName> orgs = orgService.getAllByGlobal();
		if (orgs.isEmpty()) {
			throw new RestExceptionHandler(Message.NO_SETTING_ORG_CONNECT_SYSTEM);
		}

		List<SentDataDto> rs = new ArrayList<>();
		StringBuffer nameError = null;
		for (IdName i : orgs) {
			List<Long> userIds = clericalOrg ? clericalOrgService.getClericalOrgByOrgId(i.getId())
					: userService.getListIdsVanThuVBDenByOrg(i.getId());
			if (userIds.isEmpty()) {
				if (nameError != null) {
					nameError.append(", ");
				} else {
					nameError = new StringBuffer();
				}
				nameError.append(i.getName());
			}
			rs.add(new SentDataDto(i.getId(), i.getName(), userIds));
		}

		if (nameError != null) {
			throw new RestExceptionHandler("Không tìm thấy văn thư đơn vị của tổ chức "
					+ nameError.toString().toUpperCase() + " trong hệ thống : " + old.getName());
		}

		// Tracking connect
		trackingService.save(new TrackingConnectOutside(old, ActionConnectEnum.REQUEST_ORG, true));
		return rs;
	}

	/**
	 * Send request get cert of list user by outside id
	 * 
	 * @param userIds
	 * @param outsideId
	 * @return
	 */
	@SuppressWarnings({ "unchecked", "rawtypes" })
	public List<LinkedHashMap<String, String>> getCertByUserId(Long outsideId, Long[] userIds) {
		if (ArrayUtils.isEmpty(userIds))
			return Collections.emptyList();
		OutsideSystem input = outsideSystemService.valid(outsideId, Message.NOT_FOUND_CONNECT_SYSTEM);
		OutsideSystemDto dto = new OutsideSystemDto();
		dto.set(input, TYPE.CERT);
		dto.setUserIds(userIds);
		RestTemplate restTemplate = new RestTemplate();
		String url = getUrlByCode(input.getDomain(), "cert");
		try {
			HttpEntity headers = getHeaders(input.getToken(), dto);
			ResponseEntity<RestResponse> response = restTemplate.exchange(url, HttpMethod.POST, headers,
					RestResponse.class);
			List<LinkedHashMap<String, String>> rp = (List<LinkedHashMap<String, String>>) response.getBody().getData();
			if (rp != null) {
				rp.forEach(i -> {
					trackingService.save(new TrackingConnectOutside(input, ActionConnectEnum.REQUEST_USER_CERT,
							i.get("label"), true));
				});
			}

			return rp;
		} catch (HttpClientErrorException e) {
			trackingService.save(new TrackingConnectOutside(input, ActionConnectEnum.REQUEST_USER_CERT, false));
			throw new RestExceptionHandler(BussinessCommon.getMessage(e, msgDf + input.getName()));
		} catch (Exception e) {
			e.printStackTrace();
			trackingService.save(new TrackingConnectOutside(input, ActionConnectEnum.REQUEST_USER_CERT, false));
			throw new RestExceptionHandler(msgDf + input.getName());
		}
	}

	/**
	 * Receive response cert of list user by outside id
	 * 
	 * @param userIds
	 * @param outsideId
	 * @return
	 */
	public List<LabelValueId<String>> getCertByUserId(OutsideSystemDto data) {
		data.valids();
		OutsideSystem old = outsideSystemService.findByDomainAndKey(data.getFrDomain(), data.getKey());
		if (old == null) {
			// Tracking connect
			trackingService.save(new TrackingConnectOutside(old, ActionConnectEnum.REQUEST_USER_CERT, false));
			throw new RestExceptionHandler(Message.NOT_FOUND_CONNECT_SYSTEM);
		}

		List<LabelValueId<String>> rs = commonService.getCertByUserId(data.getUserIds(), null, null, true);
		if (rs.isEmpty()) {
			throw new RestExceptionHandler(Message.NO_SETTING_USER_CERT);
		}

		// Tracking connect
		trackingService.save(new TrackingConnectOutside(old, ActionConnectEnum.REQUEST_USER_CERT, true));
		return rs;
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	public List<LinkedHashMap<String, String>> addMultipleEncrypts(Long outsideId, List<Encryption> encryptions) {
		if (BussinessCommon.isEmptyList(encryptions))
			return Collections.emptyList();
		OutsideSystem input = outsideSystemService.valid(outsideId, Message.NOT_FOUND_CONNECT_SYSTEM);
		OutsideSystemDto dto = new OutsideSystemDto();
		dto.set(input, TYPE.ENCRYPT);
		dto.setEncryptions(encryptions);
		dto.valids();
		RestTemplate restTemplate = new RestTemplate();
		String url = getUrlByCode(input.getDomain(), "encrypt");
		try {
			HttpEntity headers = getHeaders(input.getToken(), dto);
			ResponseEntity<RestResponse> response = restTemplate.exchange(url, HttpMethod.POST, headers,
					RestResponse.class);
			List<LinkedHashMap<String, String>> rp = (List<LinkedHashMap<String, String>>) response.getBody().getData();
			if (rp != null) {
				rp.forEach(i -> {
					trackingService.save(new TrackingConnectOutside(input, ActionConnectEnum.REQUEST_ADD_ENCRYPT,
							"Tệp tin : " + i.get("encrypt"), true));
				});
			}

			return rp;
		} catch (HttpClientErrorException e) {
			trackingService.save(new TrackingConnectOutside(input, ActionConnectEnum.REQUEST_ADD_ENCRYPT, false));
			throw new RestExceptionHandler(BussinessCommon.getMessage(e, msgDf + input.getName()));
		} catch (Exception e) {
			e.printStackTrace();
			trackingService.save(new TrackingConnectOutside(input, ActionConnectEnum.REQUEST_ADD_ENCRYPT, false));
			throw new RestExceptionHandler(msgDf + input.getName());
		}
	}

	public List<Encryption> addMultipleEncrypts(OutsideSystemDto data) {
		data.valids();
		OutsideSystem old = outsideSystemService.findByDomainAndKey(data.getFrDomain(), data.getKey());
		if (old == null) {
			// Tracking connect
			trackingService.save(new TrackingConnectOutside(old, ActionConnectEnum.REQUEST_ADD_ENCRYPT, false));
			throw new RestExceptionHandler(Message.NOT_FOUND_CONNECT_SYSTEM);
		}

		List<Encryption> rs = encryptionService.save(data.getEncryptions());
//		if (rs.isEmpty()) {
//			throw new RestExceptionHandler(Message.ERROR_ADD_ENCRYPT);
//		}

		// Tracking connect
		trackingService.save(new TrackingConnectOutside(old, ActionConnectEnum.REQUEST_ADD_ENCRYPT, true));
		return rs;
	}

	@Transactional
	@SuppressWarnings("rawtypes")
	public Boolean rejectObj(Long docId) {

		// Valid data
		Documents d = inService.valid(docId, Message.NOT_FOUND_DOC);
		User u = BussinessCommon.getUser();
		if (!userService.isVanThuVBDen(u)) {
			throw new RestExceptionHandler(Message.NO_CREATE_DOC);
		}

		if (!Boolean.TRUE.equals(d.getGlobal()))
			throw new RestExceptionHandler(Message.ERROR_OUTSIDE_OBJECT);

		TrackingObjectOutside track = trackingObjService.getOutsideIdSendObjId(docId, DocumentTypeEnum.VAN_BAN_DI);
		if (track == null || track.getOutsideId() == null) {
			throw new RestExceptionHandler(Message.ERROR_OUTSIDE_OBJECT);
		}

		OutsideSystem sys = outsideSystemService.valid(track.getOutsideId(), Message.NOT_FOUND_CONNECT_SYSTEM);
		if (sys == null) {
			throw new RestExceptionHandler(Message.NOT_FOUND_CONNECT_SYSTEM);
		}

		RejectDataDto dto = new RejectDataDto(sys, track.getObjIdSender());
		RestTemplate restTemplate = new RestTemplate();
		String url = getUrlByCode(sys.getDomain(), "reject-obj");
		errors = new ArrayList<>();
		String err = null;
		try {
			HttpEntity headers = getHeaders(sys.getToken(), dto);
			ResponseEntity<RestResponse> response = restTemplate.exchange(url, HttpMethod.POST, headers,
					RestResponse.class);
			HttpStatus status = response.getStatusCode();
			if (status == HttpStatus.OK) {

				// Add tracking document
				trackingService.save(
						new TrackingConnectOutside(sys, ActionConnectEnum.REJECT_RECEIVE_DOC, d.getPreview(), true));

				// Handle reject doc
				inService.rejectReceiveDocByOutsideId(d, null, null);

				// Inactive noti
				notiService.deactiveAllByDocIdAndDocType(d.getId(), DocumentTypeEnum.VAN_BAN_DEN);

				return true;
			}

			log.error("Error connect object to out systems : " + status.value());
			// Add tracking document
			trackingService
					.save(new TrackingConnectOutside(sys, ActionConnectEnum.REJECT_RECEIVE_DOC, d.getPreview(), false));
			err = msgDf + sys.getName();
			errors.add(err);
		} catch (HttpClientErrorException e) {
			err = BussinessCommon.getMessage(e, msgDf + sys.getName());
			errors.add(err);
			log.error(err);
		} catch (Exception e) {
			err = msgDf + sys.getName();
			errors.add(err);
			log.error(err);
		}
		return false;
	}

	public Boolean rejectObj(RejectDataDto data) {
		data.valids();
		OutsideSystem sysOut = data.getSys();
		OutsideSystem old = outsideSystemService.findByDomainAndKey(sysOut.getFrDomain(), sysOut.getKey());
		if (old == null) {
			// Tracking connect
			trackingService.save(new TrackingConnectOutside(old, ActionConnectEnum.REQUEST_ADD_ENCRYPT, false));
			throw new RestExceptionHandler(Message.NOT_FOUND_CONNECT_SYSTEM);
		}

		DocumentOut d = outService.valid(data.getObjId(), Message.NOT_FOUND_DOC);

		// Inform sender
		String orgReject = data.getOrgName() + "(" + data.getSys().getName() + ")";
//		outTrackService.save(data.getObjId(), orgReject, DocumentOutTrackingEnum.REJECT_RECEIVE);
		List<Long> userIds = clericalOrg ? clericalOrgService.getClericalOrgByOrgId(d.getOrgCreateId())
				: userService.getListIdsVanThuVBDenByOrg(d.getOrgCreateId());

		// Notification
		notiService.addAll(userIds, data.getObjId(), orgReject + ": " + d.getPreview(), DocumentTypeEnum.VAN_BAN_DI,
				NotificationHandleStatusEnum.TU_CHOI_TIEP_NHAN, ModuleCodeEnum.DRAFT_ISSUED);

		// Tracking connect
		trackingService.save(new TrackingConnectOutside(old, ActionConnectEnum.REQUEST_ADD_ENCRYPT, true));
		
		// Tracking object
		trackingObjService.save(new TrackingObjectOutside(old.getId(), data.getOrgId(), data.getOrgName(), d.getId(),
				DocumentTypeEnum.VAN_BAN_DI, true, null, DocumentOutTrackingEnum.REJECT_RECEIVE));
		return true;
	}
}
