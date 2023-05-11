package com.vz.backend.business.service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import com.vz.backend.core.domain.Organization;
import com.vz.backend.core.service.OrganizationService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import com.vz.backend.business.domain.ClericalOrg;
import com.vz.backend.business.repository.IClericalOrgRepository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.dto.ClericalWithOrgIds;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;
import com.vz.backend.core.service.UserService;

@Service
public class ClericalOrgService extends BaseService<ClericalOrg> {

	@Autowired
	private IClericalOrgRepository clericalOrgRepo;
	
	@Autowired
	private UserService userService;

	@Autowired
	private OrganizationService organizationService;

	@Override
	public IRepository<ClericalOrg> getRepository() {
		return clericalOrgRepo;
	}
	
	public void add(Long userId, List<Long> listOrgIds) {
		List<ClericalOrg> listData = new ArrayList<>();
		if (listOrgIds != null && !listOrgIds.isEmpty()) {
			ClericalOrg item;
			List<ClericalOrg> orgOfUserId = clericalOrgRepo.findByUserIdAndClientId(userId, BussinessCommon.getClientId());
			if (orgOfUserId != null && !orgOfUserId.isEmpty()) {
				for (ClericalOrg element : orgOfUserId) {
					element.setActive(false);
				}
			}
			for (Long orgId : listOrgIds) {
				if (orgOfUserId != null && !orgOfUserId.isEmpty()) {
					for (int j = 0; j < orgOfUserId.size(); j++) {
						if (orgOfUserId.get(j).getOrgId().equals(orgId)) {
							orgOfUserId.get(j).setActive(true);
							break;
						}
						// last item
						if (j == orgOfUserId.size() - 1) {
							item = new ClericalOrg();
							item.setUserId(userId);
							item.setActive(true);
							item.setOrgId(orgId);
							listData.add(item);
						}
					}
				} else {
					item = new ClericalOrg();
					item.setUserId(userId);
					item.setActive(true);
					item.setOrgId(orgId);
					listData.add(item);
				}
			}
			clericalOrgRepo.saveAll(listData);
		}
	}

	public List<Long> getClericalOrg(Long userId) {
		return clericalOrgRepo.getClericalOrgByUserIdAndClientId(userId, true, BussinessCommon.getClientId());
	}
	
	public List<Long> getClericalOrgByOrgId(Long orgId) {
		return clericalOrgRepo.getClericalOrgByOrgIdAndClientId(orgId, BussinessCommon.getClientId());
	}

	public List<User> getClericalByOrgIdAndClientId(Long orgId) {
		return clericalOrgRepo.getClericalByOrgIdAndClientId(orgId, BussinessCommon.getClientId());
	}

	public Page<ClericalWithOrgIds> getClerical(String name, Long orgId, DocumentTypeEnum docType, Pageable pageable) {
		Page<ClericalWithOrgIds> result = userService.getClerical(name, orgId, docType, pageable);
		for (ClericalWithOrgIds element : result) {
			element.setOrgIds(clericalOrgRepo.getClericalOrgByUserIdAndClientId(element.getUserInfo().getId(), true, BussinessCommon.getClientId()));
		}
		return result;
	}
	
	public boolean isClericalOrg(Long user, Long orgId) {
		return clericalOrgRepo.isClericalOrg(user, orgId, BussinessCommon.getClientId());
	}
	
	public List<User> getClericalOrgUserByOrgId(Long orgId) {
		List<Long> uIds = clericalOrgRepo.getClericalOrgByOrgIdAndClientId(orgId, BussinessCommon.getClientId());
		return userService.findByIds(uIds, true);
	}

	public List<Organization> getOrgByChil() {
		List<Organization> organizationList = clericalOrgRepo.getClericalOrgByListId(BussinessCommon.getUserId(), true, BussinessCommon.getClientId());
		return organizationList;
	}

	public String getTreeOrgByChil() {
		HashMap<String, String> hs = new HashMap<String, String>();
		List<Organization> organizationList =  getOrgByChil();
		String result = "[", name = "", nodeIcon = "fa fa-university";
		int level = 0;
		for (int index = 0; index < organizationList.size(); index++) {
			Organization rs = organizationList.get(index);
			name = rs.getName();
			level = rs.getLevel() + 1;
			result += "{" + "\"id\":" +  rs.getId()
					+ "," + "\"name\":" + "\"" + name;
			if ( rs.getParentId() != null) {
				result +=  "\"," + "\"parentId\":" + rs.getParentId();
			}
			else {
				result +=  "\"," + "\"parentId\":" + null;
			}
			result +=	"," + "\"nodeIcon\":" + "\"" + nodeIcon
					+ "\",\"children\":[";
		}
		result += "]";
		for (int i = 0; i < organizationList.size(); i++) result += "}]";
		return result;
	}

}
