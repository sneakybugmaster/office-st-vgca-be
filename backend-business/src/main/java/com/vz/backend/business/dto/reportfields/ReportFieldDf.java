package com.vz.backend.business.dto.reportfields;

import java.util.List;
import java.util.Map;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public abstract class ReportFieldDf {
	private int no;
	public abstract Map<String, ? extends Object> getKVMap();
	public abstract List<ReportFieldDf> cast(List<? extends Object> data);
}
