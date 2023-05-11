package com.vz.backend.business.domain.ecabinet;

import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.domain.BaseModel;

import lombok.Data;

/**
 * Phương án cho Vđề phiên họp
 *
 */
@Entity
@Table(name = "SOLUTION", schema = "vz")
@Data
@JsonIgnoreProperties({ "createDate", "updateDate", "createBy", "updateBy", "active", "clientId" })
public class Solutions extends BaseModel {

	private static Solutions instance;

	private Solutions() {
	}

	public static Solutions getInstance() {
		if (instance == null) {
			instance = new Solutions();
		}
		return instance;
	}

	@Column(name = "content", columnDefinition = "TEXT")
	private String content;

	@Column(name = "problem_id")
	private Long problemId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "problem_id", insertable = false, updatable = false)
	private Problem problem;
	
	@Override
	public void valids() {
		BussinessCommon.require("Nội dung phương án", this.content);
		BussinessCommon.validLengthData(this.content, "Nội dung phương án", 2000);
	}

	public void setObjId(List<Solutions> solutions, Long objId) {
		if (BussinessCommon.isEmptyList(solutions))
			return;
		solutions.forEach(i -> i.setProblemId(objId));
	}

	public void set(Solutions solution) {
		this.content = solution.getContent();
		this.problemId = solution.getProblemId();
	}

}
