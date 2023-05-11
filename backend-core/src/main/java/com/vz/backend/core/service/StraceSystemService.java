package com.vz.backend.core.service;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.Date;
import java.util.List;

import javax.persistence.EntityManagerFactory;
import javax.servlet.http.HttpServletRequest;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import com.vz.backend.core.auth.SecurityContext;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.CategoryEnum;
import com.vz.backend.core.config.DocumentStatusEnum;
import com.vz.backend.core.domain.StraceSystem;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.dto.ListObjectDto;
import com.vz.backend.core.dto.StraceAdminDto;
import com.vz.backend.core.dto.StraceDocDto;
import com.vz.backend.core.dto.StraceDto;
import com.vz.backend.core.repository.IStraceSystemRepository;
import com.vz.backend.util.DateTimeUtils;

@Service
public class StraceSystemService {

	@Autowired
	IStraceSystemRepository iStraceSystemRepository;

	@Autowired
	EntityManagerFactory entityManagerFactory;

	@Autowired
	HttpServletRequest request;

	private static final String[] HEADERS_TO_TRY = { "X-FORWARDED-FOR", "X-Forwarded-For", "Proxy-Client-IP",
			"WL-Proxy-Client-IP", "HTTP_CLIENT_IP", "HTTP_X_FORWARDED_FOR", "HTTP_X_FORWARDED"
//			, "HTTP_X_CLUSTER_CLIENT_IP", "HTTP_FORWARDED_FOR", "HTTP_FORWARDED", "HTTP_VIA", "REMOTE_ADDR" 
			};

	public String getClientIp() {
		for (String header : HEADERS_TO_TRY) {
			String ip = request.getHeader(header);
			if (ip != null && ip.length() != 0 && !"unknown".equalsIgnoreCase(ip)) {
				return ip;
			}
		}
		return request.getRemoteAddr();
	}

	private String getClientName() {
		return this.getClientIp();
	}

	public StraceSystem save(Long idContent, String action, String content, Long idCat, User user) {
		StraceSystem strace = new StraceSystem();
		User userLogin = SecurityContext.getCurrentUser();
		strace.setClientId(userLogin != null ? userLogin.getClientId() : user.getClientId());
		strace.setIpDevice(getClientIp());
		strace.setNameDevice(getClientName());
		strace.setAction(action);
		strace.setContent(content);
		strace.setContentId(idContent);
		strace.setCreateDate(new Date());
		strace.setUserName(userLogin != null ? userLogin.getUserName() : user.getUserName());
		strace.setIdCat(idCat);

		return iStraceSystemRepository.save(strace);
	}

	public StraceSystem save(Long idContent, String action, String content, Long idCat, User user, Long clientId) {
		StraceSystem strace = new StraceSystem();

		strace.setClientId(clientId);
		strace.setAction(action);
		strace.setContent(content);
		strace.setContentId(idContent);
		strace.setCreateDate(new Date());
		strace.setUserName( user.getUserName());
		strace.setIdCat(idCat);

		return iStraceSystemRepository.save(strace);
	}

	public ListObjectDto<StraceDto> search(String userName, Date startDate, Date endDate, Long idCat, Pageable page) {
		Page<StraceDto> pageRs = iStraceSystemRepository.search(userName, startDate, endDate, idCat,
				BussinessCommon.getClientId(), page);
		return BussinessCommon.paging(StraceDto.convert(pageRs));
	}

	public ListObjectDto<StraceDto> search(String userName, Date startDate, Date endDate, Long idCat, Sort sort) {
		List<StraceDto> listRs = iStraceSystemRepository.search(userName, startDate, endDate, idCat,
				BussinessCommon.getClientId(), sort);
		return BussinessCommon.convert(listRs);
	}

	public ListObjectDto<StraceAdminDto> searchAdmin(Date startDate, Date endDate, Long idCat, Integer month,
			Integer year, Integer quarterly, Pageable pageable) {
		int toMonth = DateTimeUtils.getMonthFromQuarterly(quarterly, 1);
		int frMonth = DateTimeUtils.getMonthFromQuarterly(quarterly, 2);
		Page<StraceAdminDto> pageRs = iStraceSystemRepository.searchAdmin(startDate, endDate, idCat, frMonth, toMonth,
				month, year, quarterly, pageable);
		
		return BussinessCommon.paging(StraceAdminDto.convert(pageRs));
	}

	public ListObjectDto<StraceDocDto> searchDoc(Date startDate, Date endDate, Integer month, Integer year,
			Integer quarterly, Long docType, DocumentStatusEnum status, Pageable page) {
		int toMonth = DateTimeUtils.getMonthFromQuarterly(quarterly, 1);
		int frMonth = DateTimeUtils.getMonthFromQuarterly(quarterly, 2);
		Page<StraceDocDto> pageRs = iStraceSystemRepository.searchDoc(frMonth, toMonth,
				CategoryEnum.DOCUMENT.getValue(), startDate, endDate, month, year, quarterly, docType, status, page);
		return BussinessCommon.paging(StraceDocDto.convert(pageRs));
	}
}
