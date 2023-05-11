package com.vz.backend.business.service.hstl;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import com.vz.backend.business.domain.hstl.Font;
import com.vz.backend.business.repository.hstl.IFontRepository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.dto.LabelValueId;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;

@Service
public class FontService extends BaseService<Font>{

	@Autowired
	private IFontRepository fontRepository;
	
	@Override
	public IRepository<Font> getRepository() {
		return fontRepository;
	}

	public List<LabelValueId<String>> list() {
		return fontRepository.list(BussinessCommon.getOrgId(), BussinessCommon.getClientId());
	}
	
	public Font findByOrganld(String organld, Long orgId) {
		return fontRepository.findByOrganldAndOrgIdAndClientIdAndActiveTrue(organld, orgId, BussinessCommon.getClientId());
	}
	
	public Font valid(String organld, Long orgId) {
		Font f = findByOrganld(organld, orgId);
		if (f == null) {
			throw new RestExceptionHandler(Message.INVALID_FONT_ORGANLD);
		}
		return f;
	}
	
	private void existOrganld(String organld, Long orgId) {
		Font f = findByOrganld(organld, orgId);
		if(f != null) {
			throw new RestExceptionHandler(Message.INVALID_FONT_ORGANLD);
		}
	}
	
	private void existName(String name, Long orgId) {
		Font f = fontRepository.findByFondNameAndOrgIdAndClientIdAndActiveTrue(name, orgId, BussinessCommon.getClientId());
		if(f != null) {
			throw new RestExceptionHandler(Message.INVALID_FONT_NAME);
		}
	}
	
	@Override
	public Font add(Font input) {
		input.valids();
		existName(input.getFondName(), input.getOrgId());
		existOrganld(input.getOrganld(), input.getOrgId());
		try {
			return getRepository().save(input);
		} catch (Exception e) {
			throw new RestExceptionHandler(Message.INVALID_FONT_DATA);
		}
	}

	public Font update(Long id, Font input) {
		Font f = valid(id, Message.NOT_FOUND_FONT);
		input.valids();
		if (!input.getFondName().equals(f.getFondName())) {
			existName(input.getFondName(), input.getOrgId());
		}

		if (!input.getOrganld().equals(f.getOrganld())) {
			existOrganld(input.getOrganld(), input.getOrgId());
		}

		f.set(input);

		try {
			return getRepository().save(f);
		} catch (Exception e) {
			throw new RestExceptionHandler(Message.INVALID_FONT_DATA);
		}
	}

	public Page<Font> paging(Integer page) {
		Pageable pageable = BussinessCommon.castToPageable(page);
		return fontRepository.findByOrgIdAndClientIdAndActiveTrue(BussinessCommon.getOrgId(),
				BussinessCommon.getClientId(), pageable);
	}

	public Boolean del(Long id) {
		Font f = valid(id, Message.NOT_FOUND_FONT);
		try {
			fontRepository.delete(f);
		} catch (Exception e) {
			throw new RestExceptionHandler(Message.FONT_IN_USE);
		}
		return true;
	}
}
