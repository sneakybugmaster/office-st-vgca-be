package com.vz.backend.business.controller;

import java.util.List;
import java.util.Set;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.fasterxml.jackson.databind.node.ArrayNode;
import com.vz.backend.business.domain.KPI;
import com.vz.backend.business.domain.KPIApplication;
import com.vz.backend.business.domain.KPISet;
import com.vz.backend.business.domain.KPIUser;
import com.vz.backend.business.domain.Targets;
import com.vz.backend.business.dto.kpi.KPIApplicationDto;
import com.vz.backend.business.dto.kpi.KPIConditionDto;
import com.vz.backend.business.dto.kpi.KPIDto;
import com.vz.backend.business.dto.kpi.KPISetDto;
import com.vz.backend.business.repository.IKPIApplicationRepository;
import com.vz.backend.business.service.KPIApplicationService;
import com.vz.backend.business.service.KPIService;
import com.vz.backend.business.service.KPISetService;
import com.vz.backend.business.service.KPIUserSevice;
import com.vz.backend.business.service.TargetsService;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.config.FrequencyEnum;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.config.SourceKPIEnum;
import com.vz.backend.core.dto.IdName;
import com.vz.backend.core.dto.LabelValueDto;

@RequestMapping("/kpi")
@RestController
public class KPIController {
	
	@Autowired
	KPIService kpiService;
	
	@Autowired
	KPISetService kpiSetService;
	
	@Autowired
	KPIApplicationService kpiAppService;

	@Autowired
	KPIUserSevice kpiUserService;
	
	@Autowired
	TargetsService kpiTargetService;
	
	@Autowired
	IKPIApplicationRepository kpiAppRepository;
	
	/**
	 * for add or update
	 * @param kpi
	 * @return
	 */
	@PostMapping("/add")
	public ResponseEntity<KPI> add(@RequestBody KPI kpi) {
		return new ResponseEntity<>(kpiService.add(kpi), HttpStatus.OK);
	}
	
	@PostMapping("/list")
	public ResponseEntity<Page<KPI>> list(@RequestBody KPIDto dto) {
		return new ResponseEntity<>(kpiService.list(dto), HttpStatus.OK);
	}
	
	@GetMapping("/all")
	public ResponseEntity<List<KPI>> kpiAll() {
		return new ResponseEntity<>(kpiService.list(), HttpStatus.OK);
	}
	
	@GetMapping("/frequency")
	public ResponseEntity<List<LabelValueDto<String>>> frequency() {
		return new ResponseEntity<>(FrequencyEnum.get(), HttpStatus.OK);
	}
	
	@GetMapping("/type_obj")
	public ResponseEntity<List<LabelValueDto<String>>> typeObj() {
		return new ResponseEntity<>(DocumentTypeEnum.get(DocumentTypeEnum.KPI), HttpStatus.OK);
	}
	
	@GetMapping("/source")
	public ResponseEntity<List<LabelValueDto<String>>> source() {
		return new ResponseEntity<>(SourceKPIEnum.get(), HttpStatus.OK);
	}
	
	/**
	 * for add or update
	 * @param kpi-set
	 * @return
	 */
	@PostMapping("/set/add")
	public ResponseEntity<KPISet> addSet(@RequestBody KPISet set) {
		return new ResponseEntity<> (kpiSetService.add(set), HttpStatus.OK);
	}
	
	@PostMapping("/set/list")
	public ResponseEntity<Page<KPISet>> listSet(@RequestBody KPISetDto dto) {
		return new ResponseEntity<>(kpiSetService.list(dto), HttpStatus.OK);
	}
	
	/**
	 * for all
	 * @return
	 */
	@PostMapping("/set/all")
	public ResponseEntity<List<KPISetDto>> setAll() {
		return new ResponseEntity<>(kpiSetService.list(), HttpStatus.OK);
	}
	
	@GetMapping("/set/{id}")
	public ResponseEntity<KPISet> getSet(@PathVariable Long id) {
		KPISet kpiSet = kpiSetService.getOne(id);
		return new ResponseEntity<>(kpiSet, HttpStatus.OK);
	}
	
	@GetMapping("/set/del/{id}")
	public ResponseEntity<Boolean> delSet(@PathVariable Long id) {
		KPISet kpiSet = kpiSetService.valid(id, Message.NOT_FOUND_OBJECT);
		kpiSet.setActive(false);
		kpiSetService.save(kpiSet);
		return new ResponseEntity<>(true, HttpStatus.OK);
	}
	
	@GetMapping("/set/del/target/{id}")
	public ResponseEntity<Boolean> delSetTarget(@PathVariable Long id) {
		Targets targets = kpiTargetService.valid(id, Message.NOT_FOUND_OBJECT);
		targets.setActive(false);
		kpiTargetService.save(targets);
		return new ResponseEntity<>(true, HttpStatus.OK);
	}
	
	/**
	 * for add or update
	 * @param kpi-app
	 * @return
	 */
	@PostMapping("/app/add")
	public ResponseEntity<KPIApplication> addApp(@RequestBody KPIApplication app) {
		return new ResponseEntity<> (kpiAppService.add(app), HttpStatus.OK);
	}
	
	@PostMapping("/app/list")
	public ResponseEntity<Page<KPIApplication>> listApp(@RequestBody KPIApplicationDto dto) {
		return new ResponseEntity<>(kpiAppService.list(dto), HttpStatus.OK);
	}
	
	@GetMapping("/app/all")
	public ResponseEntity<List<KPIApplicationDto>> appAll(@RequestParam(required = false) Integer year) {
		return new ResponseEntity<>(kpiAppService.list(year), HttpStatus.OK);
	}
	
	@PostMapping("/app/{id}")
	public ResponseEntity<KPIApplication> detailApp(@PathVariable Long id,
			@RequestBody KPIConditionDto dto) {
		return new ResponseEntity<>(kpiAppService.get(id, dto), HttpStatus.OK);
	}
	
	@GetMapping("/app/del/{id}")
	public ResponseEntity<Boolean> delApp(@PathVariable Long id) {
		KPIApplication k = kpiAppService.valid(id, Message.NOT_FOUND_KPI_APP);
		k.setActive(false);
		kpiAppRepository.save(k);
		return new ResponseEntity<>(true, HttpStatus.OK);
	}
	
	/**
	 * update target for user corresponding
	 * @param kpiUsers
	 * @return
	 */
	@PostMapping("/kpi_user/update")
	public ResponseEntity<List<KPIUser>> updateKPIUser(@RequestBody List<KPIUser> kpiUsers) {
		return new ResponseEntity<> (kpiUserService.save(kpiUsers), HttpStatus.OK);
	}
	
	@PostMapping("/statistical")
	public ResponseEntity<ArrayNode> statistical(@RequestBody KPIConditionDto dto) {
		return new ResponseEntity<>(kpiAppService.statistical(dto), HttpStatus.OK);
	}
	
	@GetMapping("/statistical/{appId}")
	public ResponseEntity<ArrayNode> statistical(@PathVariable Long appId) {
		return new ResponseEntity<>(kpiAppService.statistical(appId), HttpStatus.OK);
	}
	
	@GetMapping("/app/{appId}/users")
	public ResponseEntity<Set<IdName>> getUsersByKpiAppId(@PathVariable Long appId) {
		return new ResponseEntity<>(kpiAppService.getUsersByKpiAppId(appId), HttpStatus.OK);
	}
}
