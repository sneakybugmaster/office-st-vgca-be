package com.vz.backend.business.service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.vz.backend.business.config.ReceiveTypeEnum;
import com.vz.backend.business.domain.KPIApplication;
import com.vz.backend.business.domain.KPISet;
import com.vz.backend.business.domain.KPIUser;
import com.vz.backend.business.dto.kpi.KPIApplicationDto;
import com.vz.backend.business.dto.kpi.KPIConditionDto;
import com.vz.backend.business.dto.kpi.KPIDataDto;
import com.vz.backend.business.dto.kpi.KPIResultDto;
import com.vz.backend.business.repository.IKPIApplicationRepository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.FrequencyEnum;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.config.ModuleCodeEnum;
import com.vz.backend.core.domain.Organization;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.dto.IdName;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;
import com.vz.backend.core.service.OrganizationService;
import com.vz.backend.core.service.RoleService;
import com.vz.backend.core.service.UserService;
import com.vz.backend.util.DateTimeUtils;

@Service
public class KPIApplicationService extends BaseService<KPIApplication> {

	@Autowired
	IKPIApplicationRepository kpiAppRepository;

	@Autowired
	KPIUserSevice kpiUserService;

	@Autowired
	OrganizationService orgService;

	@Autowired
	UserService userService;
	
	@Autowired
	KPISetService kpiSetService;
	
	@Autowired
	DocumentInProcessService docInProcessService;
	
	@Autowired
	DocumentOutProcessService docOutProcessService;
	
	@Autowired
	TaskService taskService;
	
	@Autowired
	RoleService rService;

	@Override
	public IRepository<KPIApplication> getRepository() {
		return kpiAppRepository;
	}

	@Override
	public KPIApplication add(KPIApplication k) {
		// valid
		k.valids();
		validUserOrg(k);
		
		//save data
		return k.getId() == null ? save(k) : update(k);
	}
	
	@Override
	public KPIApplication save (KPIApplication k) {
		List<Long> uIdGroupUsers = k.userIds;
		List<Long> uIdGroupOrgs = userService.findByOrgIds(k.orgIds);
		KPIApplication kpiApp = new KPIApplication();
		kpiApp.set(k);
		try {
			k = kpiAppRepository.save(kpiApp);
			KPISet kpiSet = kpiSetService.valid(k.getKpiSet(), Message.KPI_SET_NOT_FOUND);
			kpiUserService.save(k.getId(), uIdGroupUsers, kpiSet.getKpis(), ReceiveTypeEnum.USER);
			kpiUserService.save(k.getId(), uIdGroupOrgs, kpiSet.getKpis(), ReceiveTypeEnum.ORG);
			return kpiApp;
		} catch (Exception e) {
			e.printStackTrace();
			throw new RestExceptionHandler(Message.KPI_SET_DATA_INVALID);
		}
	}
	
	private List<Long> getDiffOrg(KPIApplication nApp, KPIApplication oApp) {
		List<Long> nOrgIds = nApp.orgIds;
		List<Long> oOrgIds = oApp.getOrgIds();
		List<Long> diff = new ArrayList<>();
		if (nOrgIds.equals(oOrgIds))
			return diff;
		for (Long i : oOrgIds) {
			if (!nOrgIds.contains(i))
				diff.add(i);
		}
		return diff;
	}
	
	private KPIApplication update(KPIApplication nApp) {
		KPIApplication kpiApp = valid(nApp.getId(), Message.NOT_FOUND_KPI_APP);
		List<Long> orgDiff = getDiffOrg(nApp, kpiApp);
		List<Long> uIdGroupUsers = nApp.userIds;
		List<Long> uIdGroupOrgs = userService.findByOrgIds(nApp.orgIds);
		Map<KPIUser, KPIUser> map = new HashMap<>();
		kpiApp.kpiUsers.forEach(i -> {
			KPIUser key = new KPIUser(i.getUserId(), i.getTargetId(), i.getKpiAppId());
			if (!map.containsKey(key)) {
				map.put(key, i);
			}
		});
		KPISet kpiSet = kpiSetService.valid(nApp.getKpiSet(), Message.KPI_SET_NOT_FOUND);
		kpiUserService.update(map, nApp.getId(), uIdGroupUsers, kpiSet.getKpis(), ReceiveTypeEnum.USER);
		kpiUserService.update(map, nApp.getId(), uIdGroupOrgs, kpiSet.getKpis(), ReceiveTypeEnum.ORG);
 		kpiUserService.delByOrg(orgDiff, nApp.getId());
		try {
			kpiApp.set(nApp);
			nApp = kpiAppRepository.save(kpiApp);
			return nApp;
		} catch (Exception e) {
			e.printStackTrace();
			throw new RestExceptionHandler(Message.KPI_SET_DATA_INVALID);
		}
	}

	private void validUserOrg(KPIApplication k) {
		List<Organization> orgs = orgService.getOrgByIdList(k.getOrgIds(), true);
		List<User> users = userService.findByIds(k.getUserIds(), true);

		if (orgs.size() != k.getOrgIds().size())
			throw new RestExceptionHandler(Message.NOT_FOUND_ORG);
		if (users.size() != k.getUserIds().size())
			throw new RestExceptionHandler(Message.NOT_FOUND_USER);

		orgs.forEach(i -> users.forEach(j -> {
			if (i.getId().equals(j.getOrgModel().getId()))
				throw new RestExceptionHandler(Message.KPI_SET_USER_UNIQUE);
		}));
	}

	public Page<KPIApplication> list(KPIApplicationDto dto) {
		return kpiAppRepository.list(dto, BussinessCommon.getClientId(), BussinessCommon.toPage(dto));
	}
	
	private void set (KPIApplication k) {
		List<Organization> orgs = orgService.getOrgByIdList(k.getOrgIds(), true);
		List<User> users = userService.findByIds(k.getUserIds(), true);
		k.setOrgLists(orgs);
		k.setUserLists(users);
	}
	
	public KPIApplication get(Long id, KPIConditionDto dto) {
		KPIApplication kpiApp = valid(id, Message.NOT_FOUND_KPI_APP);
		set(kpiApp);
		if (dto.getFrequency() == null) return kpiApp;
		valid(kpiApp, dto);
		dto.setDateInfo();
		return setDataKPI(kpiApp, dto);
	}
	
	private KPIApplication setDataKPI(KPIApplication kpiApp, KPIConditionDto dto) {
		List<KPIDataDto> data = getDataKPI(kpiApp, dto.getStartDate(), dto.getEndDate());
		if (!FrequencyEnum.MONTH.equals(kpiApp.getFrequency()) || dto.getMonth() != null) {
			kpiApp.kpiUsers.forEach(i -> i.setActual(dto.setCondition(i).filter(data, dto.getObjType(), null, null)));
		} else {
			Map<KPIUser, Integer> mapData = new HashMap<>();
			List<Integer> months = kpiApp.getMonthIds();
			for (Integer i : months) {
				KPIConditionDto conSub = new KPIConditionDto(kpiApp.getFrequency(), i, kpiApp.getYear(), null);
				KPIUser keyTemp;
				int actual = 0;
				int value = 0;
				for (KPIUser j : kpiApp.kpiUsers) {
					keyTemp = new KPIUser(j.getUserId(), j.getTargetId(), j.getKpiAppId());
					actual = conSub.setCondition(j).filter(data, conSub.getObjType(), conSub.getStartDate(),
							conSub.getEndDate());
					value = actual;
					if (mapData.containsKey(keyTemp)) {
						value = mapData.get(keyTemp) + actual;
					}
					mapData.put(keyTemp, value);
				}
			}
			
			kpiApp.kpiUsers.forEach(j -> {
				KPIUser keyTemp = new KPIUser(j.getUserId(), j.getTargetId(), j.getKpiAppId());
				if (mapData.containsKey(keyTemp)) {
					j.setActual(mapData.get(keyTemp));
					j.setTarget(j.getTarget() * months.size());
				}
			});
		}

		return kpiApp;
	}
	
	private void valid(KPIApplication kpiApp, KPIConditionDto dto) {
		FrequencyEnum frequency = kpiApp.getFrequency();
		Integer yearSrc = kpiApp.getYear();
		List<Integer> monthSrc = kpiApp.getMonthIds();
		Integer quarterSrc = kpiApp.getQuarter();

		if (!frequency.equals(dto.getFrequency()) || !yearSrc.equals(dto.getYear())
				|| (dto.getMonth() != null && !monthSrc.contains(dto.getMonth()) && FrequencyEnum.MONTH.equals(dto.getFrequency()))
				|| (dto.getQuarter() != null && !quarterSrc.equals(dto.getQuarter())) && FrequencyEnum.QUARTER.equals(dto.getFrequency()))
			throw new RestExceptionHandler(Message.NOT_FOUND_KPI_CONDITION);
	}
	
	private List<KPIDataDto> getDataKPI(KPIApplication kpiApp, Date startDate, Date endDate) {
		List<Long> userIds = kpiApp.kpiUsers.stream().map(KPIUser::getUserId).distinct().collect(Collectors.toList());
		return setData(userIds, startDate, endDate);
	}
	
	public List<KPIDataDto> setData(List<Long> userIds, Date startDate, Date endDate) {
		List<KPIDataDto> data = new ArrayList<>();
		List<KPIDataDto> docInList = docInProcessService.findAllByToUser(userIds, startDate, endDate);
		List<KPIDataDto> docOutList = docOutProcessService.findAllByToUser(userIds, startDate, endDate);
		List<KPIDataDto> taskList = taskService.findAllByToUser(userIds, startDate, endDate);
		data.addAll(docInList);
		data.addAll(docOutList);
		data.addAll(taskList);
		return data;
	}
	
	public List<KPIApplicationDto> list(Integer year) {
		return kpiAppRepository.list(year, BussinessCommon.getClientId());
	}

	@SuppressWarnings("deprecation")
	public ArrayNode statistical(KPIConditionDto dto) {
		dto.validStatistical();
		Long kpiAppId = dto.getKpiAppId();
		Long userId = dto.getUserId();
		KPIApplication app = valid(kpiAppId, Message.NOT_FOUND_KPI_APP);
		List<KPIUser> kpiUsers = kpiUserService.getByUserIdAndKPIAppId(userId, kpiAppId);
		if(kpiUsers.isEmpty()) throw new RestExceptionHandler(Message.NOT_FOUND_KPI_CONDITION);
		
		List<Long> userIds = new ArrayList<>();
		userIds.add(userId);
		ObjectMapper mapper = new ObjectMapper();
		ArrayNode array = mapper.createArrayNode();
		List<Integer> months = getMonths(app.getFrequency(), app.getQuarter(), app.getMonths());
		KPIConditionDto con = new KPIConditionDto(app.getFrequency(), null, app.getYear(), userId);
		List<KPIDataDto> data = setData(userIds, con.getStartDate(), con.getEndDate());

		for (Integer i : months) {
			ObjectNode object = mapper.createObjectNode();
			object.put("name", "Tháng " + i);
			KPIConditionDto conSub = new KPIConditionDto(app.getFrequency(), i, app.getYear(), dto.getUserId());
			ArrayNode arrSub = mapper.createArrayNode();
			float total = 0;
			int actual = 0;
			for (KPIUser j : kpiUsers) {
				ObjectNode objSub = mapper.createObjectNode();
				objSub.put("kpiName", j.getTargets().getName());
				objSub.put("target", j.getTarget());
				actual = conSub.setCondition(j).filter(data, conSub.getObjType(), conSub.getStartDate(),
						conSub.getEndDate());
				j.setActual(actual);
				objSub.put("actual", actual);
				total = total + j.toExchange();
				arrSub.add(objSub);
			}
			object.put("total", (float) Math.round(total * 100) / 100);
			object.put("point", app.getFormula().calculator(total));
			object.put("kpis", arrSub);
			array.add(object);
		}

//		ObjectNode userInfo = mapper.createObjectNode();
//		userInfo.put("fullName", kpiUsers.get(0).getUser().getFullName());
//		userInfo.put("org", kpiUsers.get(0).getUser().getOrgModel().getName());
//		userInfo.put("position", kpiUsers.get(0).getUser().getPositionModel().getName());
//		array.add(userInfo);
		return array;
	}
	
	/**
	 * get months of frequency 
	 * @param frequency
	 * @param value : value is quarter number if frequency is quarter
	 * @return
	 */
	private List<Integer> getMonths(FrequencyEnum frequency, Integer value, String monthStr) {
		List<Integer> months = new ArrayList<>();
		switch (frequency) {
		case MONTH:
			months = BussinessCommon.toListInteger(monthStr);
			break;
		case QUARTER:
			if (value == null)
				throw new RestExceptionHandler(Message.ERROR_SYS);
			months = DateTimeUtils.getMonthFromQuarter(value);
			break;
		case SIX_MONTHS_FIRST_YEAR:
			months = Arrays.asList(1, 2, 3, 4, 5, 6);
			break;
		case SIX_MONTHS_LAST_YEAR:
			months = Arrays.asList(7, 8, 9, 10, 11, 12);
			break;
		case YEAR:
			months = Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12);
			break;
		default:
			break;
		}
		Collections.sort(months);
		return months;
	}
	
	private List<Long> getLimitUsers(User user) {
		List<Long> limitUser = new ArrayList<>();
		boolean isSuperAdmin = rService.isAllowModule(user, ModuleCodeEnum.KPI_SETUP.getName());
		if (isSuperAdmin) {
			limitUser = null;
		} else if (user.isLead()) {
			limitUser.add(user.getOrg());
			limitUser = userService.findByOrgIds(limitUser);
		} else {
			limitUser.add(user.getId());
		}
		return limitUser;
	}

	public ArrayNode statistical(Long id) {
		KPIApplication kpiApp = valid(id, Message.NOT_FOUND_KPI_APP);
		KPIConditionDto dto = new KPIConditionDto(kpiApp.getFrequency(), null, kpiApp.getYear(), null);
		ObjectMapper mapper = new ObjectMapper();
		ArrayNode array = mapper.createArrayNode();
		List<Long> userIds = getLimitUsers(BussinessCommon.getUser());

		for (KPIResultDto i : setDataKPI(kpiApp, dto).getKpiUsers()) {
			if (userIds != null && !userIds.contains(i.getUserId())) continue;
			ObjectNode object = mapper.createObjectNode();
			object.put("userId", i.getUserId());
			object.put("fullName", i.getFullName());
			object.put("positionName", i.getPositionName());
			object.put("orgName", i.getOrgName());
			object.put("total", i.getTotal());
			object.put("point", i.getPoint());
			array.add(object);
		}
		return array;
	}
	
	/**
	 * get list user by kpi app
	 * @param id
	 * @return
	 */
	public Set<IdName> getUsersByKpiAppId(Long id) {
		Set<IdName> users = new HashSet<>();
		KPIApplication kpiApp = valid(id, Message.NOT_FOUND_KPI_APP);
		List<KPIUser> kpiUsers = kpiApp.kpiUsers;
		if (kpiUsers.isEmpty())
			return users;
		List<User> usersList = kpiUsers.stream().map(KPIUser::getUser).distinct().collect(Collectors.toList());
		usersList.forEach(i -> users.add(new IdName(i.getId(), i.getFullName())));
		return users;
	}
}
