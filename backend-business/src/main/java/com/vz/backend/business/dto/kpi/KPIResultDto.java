package com.vz.backend.business.dto.kpi;

import java.util.ArrayList;
import java.util.List;

import com.vz.backend.business.domain.KPIUser;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.dto.UserBasicDto;

import lombok.Getter;
import lombok.Setter;

@Setter
@Getter
public class KPIResultDto extends UserBasicDto {
	private Long userId;
	private List<KPIUser> kpis = new ArrayList<>();

	/**
	 * Tổng cộng của 3 điểm quy đổi
	 */
	private float total = 0;

	/**
	 * Kết quả dựa trên công thức
	 */
	private String point;

	public KPIResultDto(User user) {
		super(user);
		this.userId = user.getId();
	}

	public void setTotal(List<KPIUser> kpis) {
		float temp = 0;
		for (KPIUser i : kpis) {
			temp = temp + i.toExchange();
		}
		this.total = (float) Math.round(temp * 100) / 100;
	}
}
