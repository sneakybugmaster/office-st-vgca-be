package com.vz.backend.core.service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.WeakHashMap;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Constant;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.config.ModuleCodeEnum;
import com.vz.backend.core.config.SystemEnum;
import com.vz.backend.core.domain.Module;
import com.vz.backend.core.domain.Role;
import com.vz.backend.core.dto.ModuleDto;
import com.vz.backend.core.repository.IModuleRepository;
import com.vz.backend.core.repository.IRepository;
import org.springframework.transaction.annotation.Transactional;

/**
 * @author DucND
 * @date May 07, 2020
 */
@Service
public class ModuleService extends BaseService<Module> {

	@Autowired
	private IModuleRepository moduleRepository;

	@Override
	public IRepository<Module> getRepository() {
		return moduleRepository;
	}

	@Override
	public List<Module> findByClientId(Long clientId) {
		return moduleRepository.findByActiveAndClientId(true, clientId);
	}

	/**
	 * Lấy danh sách chức năng dưới dạng tree, được map theo danh sách vai trò
	 *
	 * @param roleList
	 * @param parentId
	 * @return
	 */
	public List<Module> findByRoleList(List<Role> roleList) {
		List<Module> result = moduleRepository.findByActiveAndRoleAndParentId(roleList, true);
		return autoTreeModule(result);
	}

	//	public List<Module> findAllByRoleList(List<Role> roleList) {
	//		log.error("findByRoleList parent id: {}", parentId);
	//		List<Module> result = moduleRepository.findByActiveAndRoleAndParentId(roleList, true, parentId);
	//		if (result != null && !result.isEmpty()) {
	//			for (Module element : result) {
	//				element.setSubModule(findByRoleList(roleList, element.getId()));
	//			}
	//		}
	//		return result;
	//	}

	public List<Module> findByRoleIdList(List<Long> roleList, Long parentId) {
		List<Module> result = moduleRepository.findByActiveAndRoleIdAndParentId(roleList, true, parentId);
		if (result != null && !result.isEmpty()) {
			for (Module element : result) {
				element.setSubModule(findByRoleIdList(roleList, element.getId()));
			}
		}
		return result;
	}

	public List<Module> findAdminModuleForAdminCabinet(Long clientId) {
		List<Module> allModule = moduleRepository.findAdminModuleForAdminCabinet(clientId);
		return autoTreeModule(allModule);
	}

	public List<Module> findByClientIdAndParentId(Long clientId) {
		List<Module> allModule = moduleRepository.findByClientIdOrDefault(true, clientId);
		return autoTreeModule(allModule);
	}

	private List<Module> autoTreeModule(List<Module> allModule) {
		allModule.sort((a, b) -> {
			Long aOrder = a.getOrderNumber();
			Long bOrder = b.getOrderNumber();
			if (aOrder == null && bOrder == null) {
				return 0;
			}
			if (aOrder == null) {
				return -1;
			}
			if (bOrder == null) {
				return 1;
			}
			return (int) (bOrder - aOrder);
		});
		Map<Long, Module> myModule = new WeakHashMap<>();
		allModule.forEach(module -> {
			module.setSubModule(new ArrayList<>());
			myModule.put(module.getId(), module);
		});

		List<Module> result = new ArrayList<>();
		allModule.forEach(module -> {
			if (module.getParentId() == null) {
				result.add(module);
				return;
			}
			Module parent = myModule.get(module.getParentId());
			if (parent != null) {
				parent.getSubModule().add(module);
			}
		});
		return result;
	}

	public List<Module> findByActiveAndParentId(Boolean active, Long parentId) {
		List<Module> result = moduleRepository.findByActiveAndParentId(active, parentId);
		if (result != null && !result.isEmpty()) {
			for (Module element : result) {
				element.setSubModule(findByActiveAndParentId(active, element.getId()));
			}
		}
		return result;
	}

	public Module findByActiveAndId(Boolean active, Long id) {
		Module m = moduleRepository.findByActiveAndId(active, id);
		if (Boolean.TRUE.equals(m.getIsParent())) {
			m.setSubModule(findByActiveAndParentId(true, id));
		}
		return m;
	}

	public boolean existModuleByRoleId(ModuleCodeEnum code, long roleId) {
		return moduleRepository.existModuleByRoleId(code.getName(), roleId, true, BussinessCommon.getClientId());
	}

	public List<ModuleDto> getTreeModulesByParentIdAndClientId(Long parentId, Long clientId) {
		List<ModuleDto> allModule = moduleRepository.findModuleDtoByClientIdAndActive(clientId, true);
		allModule.sort((a, b) -> {
			Long aOrder = a.getOrderNumber();
			Long bOrder = b.getOrderNumber();
			if (aOrder == null && bOrder == null) {
				return 0;
			}
			if (aOrder == null) {
				return -1;
			}
			if (bOrder == null) {
				return 1;
			}
			return (int) (bOrder - aOrder);
		});
		Map<Long, ModuleDto> myModule = new HashMap<>();
		allModule.forEach(module -> {
			module.setSubModule(new ArrayList<>());
			myModule.put(module.getId(), module);
		});

		List<ModuleDto> result = new ArrayList<>();
		allModule.forEach(module -> {
			if (parentId == null && module.getParentId() == null || module.getParentId().equals(parentId)) {
				result.add(module);
				return;
			}
			ModuleDto parent = myModule.get(module.getParentId());
			if (parent != null) {
				parent.getSubModule().add(module);
			}
		});

		return result;
	}

	private HashMap<Long, ModuleDto> converTreeToMap(List<ModuleDto> treeModules) {
		HashMap<Long, ModuleDto> result = new HashMap<>();
		if (treeModules != null && !treeModules.isEmpty()) {
			for (ModuleDto child : treeModules) {
				result.put(child.getId(), child);
				result.putAll(converTreeToMap(child.getSubModule()));
			}
		}
		return result;
	}

	@Transactional
	public Boolean updateOrderNumber(List<ModuleDto> treeModules) {
		Long clientId = BussinessCommon.getClientId();
		HashMap<Long, ModuleDto> map = converTreeToMap(treeModules);
		List<ModuleDto> listModule = moduleRepository.findModuleDtoByClientIdAndActive(clientId, true);
		for (ModuleDto record : listModule) {
			long id = record.getId();
			ModuleDto input = map.get(id);
			if (input == null) {
				continue;
			}
			if (!input.getHide().equals(record.getHide()) || record.getOrderNumber() == null
					|| !record.getOrderNumber().equals(input.getOrderNumber())) {
				moduleRepository.updateOrderNumberByIdAndClientId(id, input.getOrderNumber(), input.getHide(),
						clientId);
			}
		}

		return true;
	}

	@Transactional
	public Boolean updateOrderNumberById(Long moduleId, Long orderNumber, Boolean hide) {
		Long clientId = BussinessCommon.getClientId();
		hide = hide == null ? false : hide;
		moduleRepository.updateOrderNumberByIdAndClientId(moduleId, orderNumber, hide, clientId);
		return true;
	}

	public Module addUsageModule(Module m) {
		Module parent = moduleRepository.findFirstByCodeAndClientIdAndActive(Constant.USER_MANUAL_CODE, BussinessCommon.getClientId(), true);
		if (parent == null) {
			parent = new Module();
			parent.setCode(Constant.USER_MANUAL_CODE);
			parent.setName(Constant.USER_MANUAL_NAME);
			moduleRepository.save(parent);
		}
		m.setParentId(parent.getId());
		m.setCode(Constant.USER_MANUAL_CODE + "__" + new Date());
		m.setRouterPath("/");
		m.setIsDefault(true);
		m.setIsParent(false);
		return moduleRepository.save(m);
	}

	public List<Module> findByParentCode (String code) {
		return moduleRepository.findByParentCode(code, BussinessCommon.getClientId());
	}

	public Module findByCode(String code) {
		return moduleRepository.findByCode(code);
	}

	public Module editUsageModule(Long id, String nName) {
		Module old = valid(id, Message.NOT_FOUND_OBJECT);
		if(!old.getName().equalsIgnoreCase(nName)) {
			old.setName(nName);
			moduleRepository.save(old);
		}
		return old;
	}

	public boolean delUsageModule(Long id) {
		Module old = valid(id, Message.NOT_FOUND_OBJECT);
		old.setActive(false);
		moduleRepository.save(old);
		return true;
	}
	
	public Module getByCode(String code) {
		return moduleRepository.findFirstByCodeAndClientIdAndActiveTrue(code, BussinessCommon.getClientId());
	}

	public List<Module> findByClientIdAndSiteAndActiveTrue(Long clientId, SystemEnum site) {
		return moduleRepository.findByClientIdAndSiteAndActiveTrue(clientId, site);
	}
	
	/**
	 * Inactive cabinet module
	 */
	@Transactional
	public void inactiveCabinetAdminModule() {
		String[] code = { Constant.CABINET_ADMIN, Constant.CABINET_ADMIN_ROLES, Constant.CABINET_ADMIN_ROOMS,
				Constant.CABINET_ADMIN_THREADS, Constant.CABINET_ADMIN_USERS, Constant.CABINET_GROUP };
		moduleRepository.inactiveModule(Arrays.asList(code));
	}
}
