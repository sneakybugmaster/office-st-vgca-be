package com.vz.backend.core.controller;

import java.util.Calendar;
import java.util.Date;

import javax.websocket.server.PathParam;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.data.jpa.domain.JpaSort;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.format.annotation.DateTimeFormat.ISO;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Constant;
import com.vz.backend.core.config.DocumentStatusEnum;
import com.vz.backend.core.service.CategoryService;
import com.vz.backend.core.service.StraceSystemService;
import com.vz.backend.util.DateTimeUtils;

@RestController
@RequestMapping(value = "/log")
public class StraceSystemController {

	@Autowired
	CategoryService catService;

	@Autowired
	private StraceSystemService straceService;

	enum SortBy {
		CONTENT("content"), // nội dung
		CREATE_DATE("createDate"), // thời gian
		USERNAME("userName"), // tên đăng nhập
		DEVICE_IP("ipDevice"), // ip thiết bị
		DEVICE_NAME("nameDevice"), // tên thiết bị
		ACTION("action"), // hành động
		CLIENT_NAME("c.name"), // tên khách hàng
		OBJECT_TYPE("c.name"), // loại đối tượng
		DOC_TYPE_NAME("d.docType.name"), // loại văn bản
		DOC_STATUS_NAME("d.status"), // trạng thái văn bản
		ACCESS_COUNT("count(s.ipDevice)"), // Số lần truy cập
		DOC_COUNT("count(s.ipDevice)"); // Số lượng văn bản

		private String field;

		private SortBy(String field) {
			this.field = field;
		}

		public static String getEnum(String name, int type) {
			for (SortBy v : values()) {
				if (v.name().equals(name)) {
					return v.field;
				}
			}
			return type == 1 ? SortBy.CREATE_DATE.field : SortBy.CLIENT_NAME.field;
		}
	}

	/**
	 * Get list log info
	 *
	 * @param username
	 * @param startDate must be format yyyy-MM-dd
	 * @param endDate   must be format yyyy-MM-dd
	 * @param idCat
	 * @param page
	 * @return list log info found
	 */
	@GetMapping(path = "/search")
	public ResponseEntity<?> getList(@RequestParam(defaultValue = "CREATE_DATE") String sortBy,
			@RequestParam(defaultValue = Constant.DEFAULT_DIRECTION) Direction direction,
			@RequestParam(name = "username", required = false) String username,
			@PathParam(value = "startDate") @DateTimeFormat(iso = ISO.DATE) Date startDate,
			@PathParam(value = "endDate") @DateTimeFormat(iso = ISO.DATE) Date endDate,
			@PathParam(value = "idCat") Long idCat,
			@RequestParam(value = "size", defaultValue = Constant.DEFAULT_PAGE_SIZE) int size,
			@RequestParam(value = "page", defaultValue = Constant.DEFAULT_PAGE_NUMBER) int page) {
		username = BussinessCommon.convert(username);
		startDate = DateTimeUtils.handleSubmit(startDate, Calendar.MILLISECOND, -1);
		endDate = DateTimeUtils.handleSubmit(endDate, Calendar.DAY_OF_MONTH, 1);
		Pageable pageable = BussinessCommon.castToPageable(page, Sort.by(direction, SortBy.getEnum(sortBy, 1)), size);
		return new ResponseEntity<>(straceService.search(username, startDate, endDate, idCat, pageable), HttpStatus.OK);
	}

	@GetMapping(path = "/export")
	public ResponseEntity<?> export(@RequestParam(defaultValue = "CREATE_DATE") String sortBy,
			@RequestParam(defaultValue = Constant.DEFAULT_DIRECTION) Direction direction,
			@RequestParam(name = "username", required = false) String username,
			@PathParam(value = "startDate") @DateTimeFormat(iso = ISO.DATE) Date startDate,
			@PathParam(value = "endDate") @DateTimeFormat(iso = ISO.DATE) Date endDate,
			@PathParam(value = "idCat") Long idCat) {
		username = BussinessCommon.convert(username);
		startDate = DateTimeUtils.handleSubmit(startDate, Calendar.MILLISECOND, -1);
		endDate = DateTimeUtils.handleSubmit(endDate, Calendar.DAY_OF_MONTH, 1);
		Sort sort = Sort.by(direction, SortBy.getEnum(sortBy, 1));
		return new ResponseEntity<>(straceService.search(username, startDate, endDate, idCat, sort), HttpStatus.OK);
	}

	/**
	 * @param startDate
	 * @param endDate
	 * @param idCat     is customer type
	 * @param quarterly
	 * @param month
	 * @param year
	 * @param page
	 * @return Loại khách hàng thuộc danh mục ADD client : có field chọn loại khách
	 *         hàng -> list này get ra từ danh mục loại khách hàng với code : LKH
	 *         findByClientIdAndCategoryCode -> categoryService
	 */
	@GetMapping(path = "/searchAdmin")
	public ResponseEntity<?> getListAdmin(@RequestParam(defaultValue = "CLIENT_NAME") String sortBy,
			@RequestParam(defaultValue = Constant.DEFAULT_DIRECTION) Direction direction,
			@PathParam(value = "startDate") @DateTimeFormat(iso = ISO.DATE) Date startDate,
			@PathParam(value = "endDate") @DateTimeFormat(iso = ISO.DATE) Date endDate,
			@PathParam(value = "idCat") Long idCat, @PathParam(value = "quarterly") Integer quarterly,
			@PathParam(value = "month") Integer month, @PathParam(value = "year") Integer year,
			@RequestParam(value = "size", defaultValue = Constant.DEFAULT_PAGE_SIZE) int size,
			@RequestParam(value = "page", defaultValue = Constant.DEFAULT_PAGE_NUMBER) int page) {
		startDate = DateTimeUtils.handleSubmit(startDate, Calendar.MILLISECOND, -1);
		endDate = DateTimeUtils.handleSubmit(endDate, Calendar.DAY_OF_MONTH, 1);
		Pageable pageable = BussinessCommon.castToPageable(page, JpaSort.unsafe(direction, SortBy.getEnum(sortBy, 2)),
				size);
		return new ResponseEntity<>(
				straceService.searchAdmin(startDate, endDate, idCat, month, year, quarterly, pageable), HttpStatus.OK);
	}

	/***
	 * search list document by client, status, time
	 *
	 * @param startDate
	 * @param endDate
	 * @param quarterly
	 * @param month
	 * @param year
	 * @param docType
	 * @param status
	 * @param page
	 * @return
	 */
	@GetMapping(path = "/searchDoc")
	public ResponseEntity<?> getListDoc(@RequestParam(defaultValue = "CLIENT_NAME") String sortBy,
			@RequestParam(defaultValue = Constant.DEFAULT_DIRECTION) Direction direction,
			@PathParam(value = "startDate") @DateTimeFormat(iso = ISO.DATE) Date startDate,
			@PathParam(value = "endDate") @DateTimeFormat(iso = ISO.DATE) Date endDate,
			@PathParam(value = "quarterly") Integer quarterly, @PathParam(value = "month") Integer month,
			@PathParam(value = "year") Integer year, @PathParam(value = "docType") Long docType,
			@PathParam(value = "status") String status,
			@RequestParam(value = "size", defaultValue = Constant.DEFAULT_PAGE_SIZE) int size,
			@RequestParam(value = "page", defaultValue = Constant.DEFAULT_PAGE_NUMBER) int page) {
		Pageable pageable = BussinessCommon.castToPageable(page, JpaSort.unsafe(direction, SortBy.getEnum(sortBy, 2)),
				size);
		return new ResponseEntity<>(straceService.searchDoc(startDate, endDate, month, year, quarterly, docType,
				DocumentStatusEnum.getEnum(status), pageable), HttpStatus.OK);
	}
}
