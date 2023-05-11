package com.vz.backend.business.service;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import javax.persistence.EntityManagerFactory;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.vz.backend.business.domain.DocumentOutComment;
import com.vz.backend.business.domain.TaskAttachment;
import com.vz.backend.business.domain.TaskComment;
import com.vz.backend.business.repository.ITaskCommentRepository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Constant;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;

/**
 * @author DucND
 * @date May 29, 2020
 */
@Service
public class TaskCommentService extends BaseService<TaskComment> {

	@Autowired
	ITaskCommentRepository repository;

	@Autowired
	EntityManagerFactory entityManagerFactory;

	@Autowired
	TaskAttachmentService attachService;

	@Override
	public IRepository<TaskComment> getRepository() {
		return repository;
	}

	/**
	 * Danh sách comment của công việc
	 *
	 * @param userId
	 * @param status
	 * @param page
	 * @return
	 */
	public List<TaskComment> getComments(Long taskId) {
		List<TaskComment> result = repository.findByTaskIdAndActive(taskId, true, BussinessCommon.getClientId());
		List<Long> ids = result.stream().map(TaskComment::getId).collect(Collectors.toList());
		if (!ids.isEmpty()) {
			List<TaskAttachment> attList = attachService.findByCommentId(ids, Constant.TYPE_TASK_CMT,
					BussinessCommon.getClientId(), true);
			if (attList.isEmpty()) {
				return result;
			}
			result.forEach(i -> {
				i.setAttachments(getAttachsByCmtId(attList, i.getId()));
			});
		}

		return result;
	}

	private List<TaskAttachment> getAttachsByCmtId(List<TaskAttachment> all, Long cmtId) {
		List<TaskAttachment> attList = new ArrayList<>();
		all.forEach(i -> {
			if (cmtId.equals(i.getObjectId())) {
				attList.add(i);
			}
		});
		return attList;
	}
	
	public Boolean deleteComment(Long cmtId, boolean showError) {
		Optional<TaskComment> dc = repository.findById(cmtId);
		if (dc.isPresent()) {
			dc.get().setActive(false);
			repository.save(dc.get());
		} else {
			if (showError) {
				throw new RestExceptionHandler(Message.ACTION_FAILED);
			}
		}
		return true;
	}
}
