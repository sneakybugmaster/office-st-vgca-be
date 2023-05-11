package com.vz.backend.business.domain.ecabinet;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.domain.BaseModel;

import lombok.Getter;
import lombok.Setter;

/**
 * Kỷ yếu phiên họp
 *
 */
@Setter
@Entity
@Table(name = "YEAR_BOOK", schema = "vz")
@JsonIgnoreProperties({ "createDate", "updateDate", "createBy", "updateBy", "active", "clientId" })
@Getter
public class YearBook extends BaseModel {

	/**
	 *
	 */
	private static final long serialVersionUID = 1L;

	private static YearBook instance;

	public YearBook() {}

	public static YearBook getInstance() {
		if (instance == null) {
			return new YearBook();
		}
		return instance;
	}

	@Column(name = "[name]")
	private String name;

	@Column(name = "parent_id")
	private Long parentId;

	@Column(name = "order_number")
	private Long orderNumber;

	@Override
	public void valids() {
		BussinessCommon.require("Kỷ yếu", this.name);
		BussinessCommon.validLengthData(this.name, "Kỷ yếu", 100);
	}

//	public List<TreeObj> converts(List<YearBook> objs) {
//		if (BussinessCommon.isEmptyList(objs))
//			return Collections.emptyList();
//		List<TreeObj> rs = new ArrayList<>();
//		for (YearBook obj : objs) {
//			rs.add(new TreeObj(obj.getId(), obj.getName(), obj.getParentId()));
//		}
//		return rs;
//	}
}
