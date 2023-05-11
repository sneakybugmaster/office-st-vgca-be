package com.vz.backend.core.thread;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.List;

import javax.naming.Context;
import javax.naming.NamingEnumeration;
import javax.naming.NamingException;
import javax.naming.directory.Attribute;
import javax.naming.directory.Attributes;
import javax.naming.directory.DirContext;
import javax.naming.directory.InitialDirContext;
import javax.naming.directory.SearchResult;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;

import com.google.gson.Gson;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Constant;
import com.vz.backend.core.controller.SamlController;
import com.vz.backend.core.domain.Category;
import com.vz.backend.core.domain.CategoryType;
import com.vz.backend.core.domain.Client;
import com.vz.backend.core.domain.Organization;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.dto.GetOrgResponse;
import com.vz.backend.core.dto.GetPostionResponse;
import com.vz.backend.core.dto.GetUserResponse;
import com.vz.backend.core.dto.OrgSyncResponse;
import com.vz.backend.core.dto.OrganizationDto;
import com.vz.backend.core.dto.PostionSyncResponse;
import com.vz.backend.core.dto.UserSynchResponse;
import com.vz.backend.core.repository.ICategoryRepository;
import com.vz.backend.core.repository.ICateroryTypeRepository;
import com.vz.backend.core.repository.IClientRepository;
import com.vz.backend.core.repository.OrganizationRepository;
import com.vz.backend.core.service.UserService;
import com.vz.backend.util.StringUtils;

import lombok.extern.slf4j.Slf4j;

/**
 * @author DucND
 * @date 21 thg 7, 2020
 */
@Slf4j
@Component
public class SynchronizeData implements Runnable {

	@Value("${sync.user:}")
	private String syncUser;

	@Value("${sync.org:}")
	private String syncOrg;

	@Value("${sync.position:}")
	private String syncPosition;

	@Value("${hldap.url:}")
	private String ldapUrl;

	@Value("${hldap.name:ou=Users,dc=vgisc,dc=com}")
	private String ldapName;

	@Value("${hldap.orgId:}")
	private Long ldapOrgId;

	@Value("${hldap.positionId:}")
	private Long ldapPositionId;

	@Value("${hldap.clientId:1}")
	private Long ldapClientId;

	@Value("${hldap.filter:(objectclass=identityPerson)}")
	private String ldapFilter;

	@Value("${hldap.mail:@vgisc.com}")
	private String ldapMail;

	private boolean showed = false;
	private boolean showedLdap = false;

	@Autowired
	private UserService userService;

	@Autowired
	private OrganizationRepository organizationRepository;

	@Autowired
	private ICategoryRepository categoryRepository;

	@Autowired
	private IClientRepository clientRepository;

	@Autowired
	private ICateroryTypeRepository categoryTypeRepository;

	private void getUserFromDC(long orgParentId, long clientId) {
		try {
			RestTemplate restTemplate = new RestTemplate();
			String result = restTemplate.getForObject(syncUser + "/" + orgParentId, String.class);
			if (result != null) {
				Gson g = new Gson();
				GetUserResponse p = g.fromJson(result, GetUserResponse.class);
				if (p.getData() != null && p.getData().getTotal() > 0) {
					log.info("START SYNC USER");
					parseUserSyncToUser(p.getData().getUsers(), clientId);
				}
			}
		} catch (Exception e) {
			log.error(e.toString());
			throw e;
		}
	}

	private void getOrgFromDC() {
		try {
			RestTemplate restTemplate = new RestTemplate();
			String result = restTemplate.getForObject(syncOrg, String.class);
			if (result != null) {
				Gson g = new Gson();
				GetOrgResponse p = g.fromJson(result, GetOrgResponse.class);
				if (p.getData() != null && p.getData().size() > 0) {
					log.info("START SYNC ORG");
					parseOrgSyncToOrg(p.getData());
				}
			}
		} catch (Exception e) {
			log.error(e.toString());
			throw e;
		}
	}

	private void getPostionFromDC(long siteId, long clientId, long categoryType) {
		try {
			RestTemplate restTemplate = new RestTemplate();
			String result = restTemplate.getForObject(syncPosition + "/" + siteId, String.class);
			if (result != null) {
				Gson g = new Gson();
				GetPostionResponse p = g.fromJson(result, GetPostionResponse.class);
				if (p.getData() != null && p.getData().size() > 0) {
					log.info("START SYNC POSITION");
					parsePostionSyncToPosition(p.getData(), clientId, categoryType);
				}
			}
		} catch (Exception e) {
			log.error(e.toString());
			throw e;
		}
	}

	private void parsePostionSyncToPosition(List<PostionSyncResponse> syncList, long clientId, long categoryType) {
		if (syncList != null && syncList.size() > 0) {
			Category item;
			PostionSyncResponse temp;
			List<Category> posList = new ArrayList<>();
			for (PostionSyncResponse element : syncList) {
				item = new Category();
				temp = element;
				item.setClientId(clientId);
				item.setCategoryTypeId(categoryType);
				item.setName(temp.getUserPoisitionName());
				item.setSyncCode(temp.getUserPoisitionId());
				item.setActive(true);
				posList.add(item);
			}
			this.savePosition(posList);
			this.delPosition(posList, clientId, categoryType);
		}
	}

	private void delPosition(List<Category> posFromSSOList, long clientId, long categoryType) {
		List<Category> posFromOffice = categoryRepository.findByClientIdAndCategoryTypeIdAndLDAP(clientId, categoryType,
				true);
		List<Category> posDelByClient = new ArrayList<>();
		HashMap<Long, Category> posFromSSOHM = new HashMap<>(); // key : code, value : position
		posFromSSOList.forEach(j -> {
			posFromSSOHM.put(j.getSyncCode(), j);
		});

		for (Category o : posFromOffice) {
			if (!posFromSSOHM.containsKey(o.getSyncCode())) {
				o.setActive(false);
				posDelByClient.add(o);
			}
		}

		if (!BussinessCommon.isEmptyList(posDelByClient)) {
			categoryRepository.saveAll(posDelByClient);
		}
		log.info("END SYNC POSITION SUCCESS FOR DELETED");
	}

	private void savePosition(List<Category> catList) {
		if (catList != null && catList.size() > 0) {
			Category item;
			Category temp = null;
			for (Category element : catList) {
				item = element;
				temp = categoryRepository.findBySyncCodeAndClientId(item.getSyncCode(), item.getClientId());
				if (temp == null) {
					temp = categoryRepository.findByClientIdAndNameAndCode(item.getClientId(), item.getName(),
							Constant.CAT_POSITION);
					if (temp == null) {
						temp = item;
					} else {
						temp.setActive(true);
						temp.setIsLdap(true);
						temp.setSyncCode(item.getSyncCode());
					}

				} else {
					temp.setName(item.getName());
					temp.setIsLdap(true);
					temp.setActive(true);
				}
				categoryRepository.save(temp);
				// tempList.add(temp);
			}
			// organizationRepository.saveAll(tempList);
			log.info("END SYNC POSITION SUCCESS FOR ADD OR UPDATE");
		}
	}

	private void parseOrgSyncToOrg(List<OrgSyncResponse> orgsSysnc) {
		if (orgsSysnc == null || orgsSysnc.isEmpty()) {
			return;
		}
		Organization item;
		OrgSyncResponse temp;
		List<Organization> orgList;
		for (int i = 0; i < orgsSysnc.size(); i++) {
			temp = orgsSysnc.get(i);
			Client client;
			List<OrgSyncResponse> children;
			if (temp != null && temp.getParentId() == 0) { // Mặc định parentId = 0 là 1 Client
				orgList = new ArrayList<>();
				client = clientRepository.findByCodeAndActive(temp.getCode(), true);
				if (client == null) {
					client = clientRepository.findByName(temp.getName());
					if (client == null) {
						client = new Client();
						client.setName(temp.getName());
						client.setCode(temp.getCode());
						client.setActive(true);
					} else {
						client.setCode(temp.getCode());
						client.setActive(true);
					}

				}

				client = clientRepository.save(client);
				children = temp.getChildren();
				children.add(temp);

				if (!BussinessCommon.isEmptyList(children)) {
					for (OrgSyncResponse subTemp : children) {
						item = new Organization();
						item.setClientId(client.getId());
						item.setName(subTemp.getName());
						item.setIsLdap(true);
						item.setCode(subTemp.getID());
						item.setOrgIdSync(subTemp.getID());
						if (subTemp.getParentId() != null && subTemp.getParentId() != 0) {
							item.setLevel(1);
							item.setParentId(subTemp.getParentId());
						} else {
							item.setLevel(0);
							item.setParentId(null);
						}
						item.setActive(true);
						orgList.add(item);

					}

					// get code for position
					CategoryType chucVu = categoryTypeRepository.findByClientIdAndCode(client.getId(),
							Constant.CAT_POSITION);
					if (chucVu == null) {
						CategoryType ct = new CategoryType();
						ct.setActive(true);
						ct.setClientId(client.getId());
						ct.setCode(Constant.CAT_POSITION);
						ct.setName(Constant.NAME_POSITION);
						chucVu = categoryTypeRepository.save(ct);
					}

					// sync
					saveOrg(orgList);
					delOrg(orgList, client.getId());
					getPostionFromDC(temp.getSiteId(), client.getId(), chucVu.getId());
					List<Organization> orgFromOffice = organizationRepository.findByClientIdAndLDAP(client.getId(), true);
					for(Organization organization : orgFromOffice){
						getUserFromDC(organization.getOrgIdSync(), client.getId());
						if(organization.getParentId() != null && organization.getParentId() > 0){
							getChildOrg(organization.getParentId(),client.getId());
						}
					}
				}
			}
		}
	}
	private void getChildOrg(long parentId,long clientId){
		Organization parent = organizationRepository.findByClientIdAndId(clientId,parentId);
		if(parent != null){
			getUserFromDC(parent.getOrgIdSync(), clientId);
			if(parent != null && parent.getParentId() > 0){
				getChildOrg(parent.getParentId(),clientId);
			}
		}
	}
	// organization deleted
	private void delOrg(List<Organization> orgFromSSOList, long clientId) {
		List<Organization> orgFromOffice = organizationRepository.findByClientIdAndLDAP(clientId, true);
		List<Organization> orgDelByClient = new ArrayList<>();
		HashMap<Long, Organization> orgFromSSOHM = new HashMap<>(); // key : code, value : org
		orgFromSSOList.forEach(j -> orgFromSSOHM.put(j.getCode(), j));

		for (Organization o : orgFromOffice) {
			if (!orgFromSSOHM.containsKey(o.getCode())) {
				o.setActive(false);
				orgDelByClient.add(o);
			}
		}

		if (!BussinessCommon.isEmptyList(orgDelByClient)) {
			organizationRepository.saveAll(orgDelByClient);
		}
		log.info("END SYNC ORG SUCCESS FOR DELETED");
	}

	private void saveOrg(List<Organization> orgList) {
		if (orgList == null || orgList.isEmpty()) {
			return;
		}
		Organization item;
		Organization temp = null;
		new ArrayList<>();
		for (Organization element : orgList) {
			item = element;
			 log.info("item.getCode(): {}", item.getCode());
			temp = organizationRepository.findByCode(item.getCode());
			if (temp == null) {
				temp = organizationRepository.findByNameAndClientIdAndParentId(item.getName(), item.getClientId(),
						item.getParentId());
				if (temp == null) {
					temp = item;
				} else {
					temp.setIsLdap(true);
					temp.setCode(item.getCode());
					temp.setActive(true);
				}

			} else {
				temp.setIsLdap(true);
				temp.setCode(item.getCode());
				// temp.setParentId(item.getParentId());
				temp.setName(item.getName());
				temp.setActive(true);
			}
			if (item.getParentId() != null) {
				Organization parent = organizationRepository.findByCode(item.getParentId());
				if (parent != null) {
					temp.setParentId(parent.getId());

				} else {
					temp.setParentId(null);
				}
			}
			organizationRepository.save(temp);
			// tempList.add(temp);

		}
		// organizationRepository.saveAll(tempList);
		log.info("END SYNC ORG SUCCESS FOR ADD OR UPDATE");
	}

	private void parseUserSyncToUser(List<UserSynchResponse> usersSysnc, long clientId) {
		if (usersSysnc == null || usersSysnc.isEmpty()) {
			return;
		}
		List<User> userList = new ArrayList<>();
		PasswordEncoder encoder = new BCryptPasswordEncoder();
		String passwordDefault = encoder.encode(Constant.PASSWORD_DEFAULT);
		usersSysnc.forEach(temp -> {
			if (temp.getOrganizationIds() != null && temp.getOrganizationIds().length > 0 && temp.getChucVuId() != null
					&& !temp.getChucVuId().equals("null")) {
				User item = new User();
				// item.setClientId(1L);// Mặc định là Cục H05
				OrganizationDto[] org = temp.getOrganizationIds();
				if (!BussinessCommon.isEmptyArr(org) && org[0] != null) {
					// item.setOrg(org[0].getID());
					Organization orgByCode = organizationRepository.findByCode(org[0].getID());
					if (orgByCode == null || orgByCode.getId() == 0) {
						log.error("Can't find org for {} with orgid: {}", temp.getUid(), org[0].getID());
						return;
					}
					item.setOrg(orgByCode.getId());
				}

				Category cat = categoryRepository.findBySyncCodeAndClientId(Long.parseLong(temp.getChucVuId()),
						clientId);
				item.setClientId(clientId);
				item.setPosition(cat != null ? cat.getId() : 213L);
				item.setPassword(passwordDefault);
				item.setChangePass(false);
				item.setLdap(true);
				item.setEmail(temp.getMail());
				item.setUserName(temp.getUid()); // Mặc định username không cho sửa
				if ("active".equals(temp.getZimbraAccountStatus())) {
					item.setActive(true);
				} else {
					item.setActive(false);
				}
				item.setPhone(temp.getMobile());
				item.setFullName(temp.getDisplayName());
				userList.add(item);
			}
		});
		this.saveUser(userList);
		this.delUser(userList, clientId);
	}

	private void delUser(List<User> userFromSSOList, long clientId) {
		List<User> userFromOffice = userService.findByClientIdAndLDAP(clientId, true);
		List<User> userDelByClient = new ArrayList<>();
		HashMap<String, User> userFromSSOHM = new HashMap<>(); // key : username, value : user
		userFromSSOList.forEach(j -> userFromSSOHM.put(j.getUserName(), j));

		for (User o : userFromOffice) {
			if (!userFromSSOHM.containsKey(o.getUserName())) {
				o.setActive(false);
				userDelByClient.add(o);
			}
		}

		if (!BussinessCommon.isEmptyList(userDelByClient)) {
			userService.saveAll(userDelByClient);
		}
		log.info("END SYNC USER SUCCESS FOR DELETED");
	}

	private void saveUser(List<User> userList) {
		if (userList != null && !userList.isEmpty()) {
			User item;
			User temp;
			List<User> tempList = new ArrayList<>();
			for (int i = 0; i < userList.size(); i++) {
				item = userList.get(i);
				temp = userService.findByUserNameForLdap(item.getUserName(), item.getClientId());
				if (temp == null) {
					temp = item;
				} else {
					temp.setFullName(item.getFullName());
					temp.setActive(item.getActive());
					temp.setEmail(item.getEmail());
					temp.setPosition(item.getPosition());
					temp.setOrg(item.getOrg());
				}
				//				if (item.getOrg() != null) {
				//					Organization org = organizationRepository.findByCode(item.getOrg());
				//					if (org != null && org.getId() != 0) {
				//						temp.setOrg(org.getId());
				//					}
				//				} else
				//					temp.setOrg(null);
				tempList.add(temp);
			}
			userService.saveAll(tempList);
			log.info("END SYNC USER SUCCESS FOR ADD OR UPDATE");
		}
	}

	@Override
	public void run() {
		syncCas();
		syncLdapWso2();
	}

	private void syncCas() {
		if (StringUtils.isNullOrEmpty(syncUser) || StringUtils.isNullOrEmpty(syncOrg)
				|| StringUtils.isNullOrEmpty(syncPosition)) {
			if (!showed) {
				log.info("Incomplete sync information: \nu: {}\no: {}\np: {}", syncUser, syncOrg, syncPosition);
				showed = true;
			}
			return;
		}
		getOrgFromDC();
	}

	private static final String LDAP_CONTENT = "com.sun.jndi.ldap.LdapCtxFactory";

	private void syncLdapWso2() {
		log.info("ldap url: {}\n", ldapUrl);
		if (StringUtils.isNullOrEmpty(ldapUrl) || StringUtils.isNullOrEmpty(ldapName)) {
			if (!showedLdap) {
				log.info("Incomplete ldap information:");
				log.info("ldap url: {}\n", ldapUrl);
				showedLdap = true;
			}
			return;
		}
		Hashtable<String, String> env = new Hashtable<>();
		env.put(Context.INITIAL_CONTEXT_FACTORY, LDAP_CONTENT);
		env.put(Context.PROVIDER_URL, ldapUrl);

		env.put(Context.SECURITY_AUTHENTICATION, "none");
		try {
			DirContext ctx = new InitialDirContext(env);
			NamingEnumeration<?> namingEnum = ctx.search(ldapName, ldapFilter, null);

			HashMap<String, User> userMap = new HashMap<>();
			List<String> userNameAll = new ArrayList<>();
			while (namingEnum.hasMore ()) {
				SearchResult result = (SearchResult) namingEnum.next ();
				Attributes attrs = result.getAttributes ();

				Attribute attr = attrs.get("cn");
				String userId = (String) attr.get();
				if (userId == null) {
					continue;
				}
				String userName = SamlController.convertNameId2Username(userId, ldapMail);
				User user = new User();
				user.setClientId(ldapClientId);
				user.setActive(true);
				user.setUserName(userName);
				user.setFullName(getKey(attrs, "givenName") + " " + getKey(attrs, "sn"));
				user.setEmail(getKey(attrs, "mail"));
				user.setPhone(getKey(attrs, "mobile"));
				user.setLdap(true);
				user.setOrg(ldapOrgId);
				user.setPosition(ldapPositionId);
				userNameAll.add(userName);
				userMap.put(userName, user);
			}

			List<String> existUser = userService.exist(userNameAll, ldapClientId);
			userService.setLdap(existUser, ldapClientId);
			existUser.forEach(userMap::remove);

			userService.saveAll(userMap.values());

			namingEnum.close();
		} catch (NamingException e) {
			e.printStackTrace();
		}
	}

	private static String getKey(Attributes attrs, String key) {
		Attribute attr = attrs.get(key);
		if (attr == null) {
			return "";
		}
		try {
			return (String) attr.get();
		} catch (NamingException e) {
			return "";
		}
	}
}
