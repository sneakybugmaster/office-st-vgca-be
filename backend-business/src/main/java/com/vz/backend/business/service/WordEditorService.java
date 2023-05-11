package com.vz.backend.business.service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.commons.lang.ArrayUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.vz.backend.business.domain.Comment;
import com.vz.backend.business.domain.TaskAttachment;
import com.vz.backend.business.domain.WordEditor;
import com.vz.backend.business.domain.WordEditorProcess;
import com.vz.backend.business.dto.PDocDto;
import com.vz.backend.business.dto.ReportDocByTypeDto;
import com.vz.backend.business.dto.TrackingReceiveDocDto;
import com.vz.backend.business.dto.TrackingSendReceiveDto;
import com.vz.backend.business.repository.ICommentRepository;
import com.vz.backend.business.repository.IWordEditorProcessRepository;
import com.vz.backend.business.repository.IWordEditorRepository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Constant;
import com.vz.backend.core.config.DocumentInHandleStatusEnum;
import com.vz.backend.core.config.DocumentStatusEnum;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.config.ModuleCodeEnum;
import com.vz.backend.core.config.NotificationHandleStatusEnum;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;
import com.vz.backend.util.StringUtils;
import org.springframework.transaction.annotation.Transactional;

@Service
public class WordEditorService extends BaseService<WordEditor> {

	@Autowired
	IWordEditorRepository wEditorRepository;

	@Autowired
	IWordEditorProcessRepository wEditorProcessRepository;

	@Autowired
	NotificationService notificationService;

	@Autowired
	ICommentRepository commentRepository;

	@Autowired
	TaskAttachmentService attService;

	@Autowired
	TaskExecuteService taskExecuteService;

	@Override
	public IRepository<WordEditor> getRepository() {
		return wEditorRepository;
	}

	public static final DocumentInHandleStatusEnum[] notYetHandleStatusArr = { DocumentInHandleStatusEnum.CHO_XU_LY,
			DocumentInHandleStatusEnum.DANG_XU_LY };
	public static final DocumentInHandleStatusEnum notYetHandleStatus = DocumentInHandleStatusEnum.CHO_XU_LY;
	public static final DocumentStatusEnum notYetStatus = DocumentStatusEnum.NOT_YET;

	public WordEditor transfer(Long weId, Long[] userIds, Long node, String comment, MultipartFile[] files) {
		Long userId = BussinessCommon.getUserId();
		WordEditor we = valid(weId);
		if(DocumentStatusEnum.DONE.equals(we.getStatus())) {
			throw new RestExceptionHandler(Message.NO_DONE_PROCESS);
		}

		// update old
		WordEditorProcess process = updateProcess(userId, weId);

		// update word editor
		updateWordEditor(we, DocumentStatusEnum.DOING, node);

		// add new
		int step = process.getStep() + 1;
		List<WordEditorProcess> pList = saveProcess(weId, userIds, node, step);
		WordEditorProcess fProcess = pList.get(0);
		fProcess.setContent(comment);

		// att
		saveCommentAndAttach1(fProcess.getId(), files);

		// notification
		notificationService.addAll(Arrays.asList(userIds), weId, we.getName(), DocumentTypeEnum.VAN_BAN_SOAN_THAO,
				NotificationHandleStatusEnum.XU_LY_CHINH, ModuleCodeEnum.TASK_MAIN);

		return we;
	}
	
	private WordEditorProcess updateProcess(Long userId, Long weId) {
		List<WordEditorProcess> rs = wEditorProcessRepository.findByToUserAndWeId(weId, userId,
				BussinessCommon.getClientId());
		
		if(BussinessCommon.isEmptyList(rs)) throw new RestExceptionHandler(Message.NO_DONE_PROCESS);
		
		rs.forEach(i -> {
			if (!isStatus(i.getHandleStatus(), notYetHandleStatusArr))
				throw new RestExceptionHandler(Message.NO_DONE_PROCESS);
			updateProcess(i, DocumentInHandleStatusEnum.DA_XU_LY);
		});
		return rs.get(0);
		
//		WordEditorProcess i = rs.get(0);
//		if (!isStatus(i.getHandleStatus(), notYetHandleStatusArr))
//			throw new RestExceptionHandler(Message.NO_DONE_PROCESS);
//		updateProcess(i, DocumentInHandleStatusEnum.DA_XU_LY);
//		
//		return i;
	}

	public boolean isStatus(DocumentInHandleStatusEnum status, DocumentInHandleStatusEnum[] arr) {
		return Arrays.stream(arr).anyMatch(status::equals);
	}

	public void saveCommentAndAttach(Long objId, MultipartFile[] files, String comment) {
		if (!StringUtils.isNullOrEmpty(comment) || !ArrayUtils.isEmpty(files)) {
			Comment cmt = new Comment(objId, DocumentTypeEnum.VAN_BAN_SOAN_THAO, comment);
			cmt = commentRepository.save(cmt);
			attService.addListAttachment(files, cmt.getId(), com.vz.backend.core.config.Constant.TYPE_WORD_EDITOR_CMT);
		}
	}

	public void saveCommentAndAttach1(Long objId, MultipartFile[] files) {
		if (!ArrayUtils.isEmpty(files)) {
			attService.addListAttachment(files, objId, com.vz.backend.core.config.Constant.TYPE_WORD_EDITOR);
		}
	}

	public WordEditorProcess updateProcess(WordEditorProcess process, DocumentInHandleStatusEnum status) {
		process.setHandleStatus(status);
		return wEditorProcessRepository.save(process);
	}

	public void updateWordEditor(WordEditor wordEditor, DocumentStatusEnum status, Long node) {
		wordEditor.setStatus(status);
		if (node != null)
			wordEditor.setNode(node);
		wEditorRepository.save(wordEditor);
	}

	private HashMap<Long, WordEditorProcess> oMap = new HashMap<>();

	public List<WordEditorProcess> saveProcess(Long weId, Long[] userIds, Long node, int step) {
		if (ArrayUtils.isEmpty(userIds))
			return Collections.emptyList();

		setMap(weId, userIds, step);

		List<WordEditorProcess> processs = new ArrayList<>();
		WordEditorProcess we;
		Long frUserId = BussinessCommon.getUserId();
		for (Long u : userIds) {
			if (this.oMap.containsKey(u)) {
				we = this.oMap.get(u);
				we.setHandleStatus(notYetHandleStatus);
			} else {
				we = new WordEditorProcess(weId, frUserId, u, node, notYetHandleStatus, step);
			}

			processs.add(we);
		}
		return wEditorProcessRepository.saveAll(processs);
	}

	private void setMap(Long weId, Long[] userIds, int step) {
		List<WordEditorProcess> oList = findByToUserAndWeId(userIds, weId, step);
		HashMap<Long, WordEditorProcess> oMap = new HashMap<>();
		oList.forEach(i -> oMap.put(i.getToUserId(), i));
	}

	public WordEditorProcess save(Long weId) {
		Long userId = BussinessCommon.getUserId();
		WordEditorProcess we = new WordEditorProcess(weId, userId, userId,
				com.vz.backend.core.config.Constant.START_NODE, DocumentInHandleStatusEnum.CHO_XU_LY,
				com.vz.backend.core.config.Constant.START_STEP);
		return wEditorProcessRepository.save(we);
	}

	public WordEditor valid(Long weId) {
		WordEditor we = wEditorRepository.findByClientIdAndId(BussinessCommon.getClientId(), weId);
		if (we == null)
			throw new RestExceptionHandler(Message.NOT_FOUND_DOC);
		return we;
	}

	public WordEditorProcess findByToUserAndWeId(Long userId, Long weId) {
		List<WordEditorProcess> rs = wEditorProcessRepository.findByToUserAndWeId(weId, userId,
				BussinessCommon.getClientId());
		return rs.isEmpty() ? null : rs.get(0);
	}

	public List<WordEditorProcess> findByToUserAndWeId(Long[] userIds, Long weId, int step) {
		return wEditorProcessRepository.findByToUserAndWeId(weId, step, userIds, BussinessCommon.getClientId());
	}

	public WordEditorProcess getProcess(Long userId, Long weId) {
		WordEditorProcess we = findByToUserAndWeId(userId, weId);
		if (we == null)
			throw new RestExceptionHandler(Message.NO_PROCESS_HANDLE);
		return we;
	}

	public WordEditor add(WordEditor wordEditor) {
		wordEditor.valid();
		Long weId = wordEditor.getId();
		boolean update = weId != null;
		if (update)
			valid(weId);

		wordEditor = wEditorRepository.save(wordEditor);
		if (!update)
			save(wordEditor.getId());
		return wordEditor;
	}

	@Transactional
	public WordEditor done(Long weId, String comment, MultipartFile[] files) {
		Long userId = BussinessCommon.getUserId();
		WordEditor we = valid(weId);

		// update old
		WordEditorProcess process = getProcess(userId, weId);
		//canDone(weId, process);
		process = updateProcess(process, DocumentInHandleStatusEnum.DA_XU_LY);

		// update word editor
		updateWordEditor(we, DocumentStatusEnum.DONE, null);

		// att
		saveCommentAndAttach1(process.getId(), files);

		// notification
		notificationService.setActiveByDocIdAndDocType(weId, DocumentTypeEnum.VAN_BAN_SOAN_THAO, false);

		return we;
	}
	
	private void canDone(Long weId, WordEditorProcess process) {
		List<WordEditorProcess> all = findByWeId(weId);
		if (all.contains(process))
			all.remove(process);
		all.forEach(i -> {
			if (isStatus(i.getHandleStatus(), notYetHandleStatusArr)) {
				throw new RestExceptionHandler(Message.NO_FINISH_TASK);
			}
		});
	}

	@Transactional
	public WordEditor get(Long weId) {
		WordEditor we = valid(weId);
		List<WordEditorProcess> pList = findByWeId(weId);
		List<Long> pIds = pList.stream().map(WordEditorProcess::getId).collect(Collectors.toList());
		List<TaskAttachment> aList = attService.findByCommentId(pIds, Constant.TYPE_WORD_EDITOR,
				BussinessCommon.getClientId(), true);
		we.setAttachments(aList);
		WordEditorProcess process = findByToUserAndWeId(BussinessCommon.getUserId(), weId);
		if (!DocumentStatusEnum.DONE.equals(we.getStatus()) && process != null) {
			we.setHandleStatus(process.getHandleStatus());
			we.setNode(process.getNode());
		}
		notificationService.setReadByDocIdAndDocTypeAndUserId(true, weId, DocumentTypeEnum.VAN_BAN_SOAN_THAO,
				BussinessCommon.getUserId());
		return we;
	}

	public Page<WordEditor> list(String text, Date startDate, Date endDate, DocumentStatusEnum status,
			Pageable pageable) {
		return wEditorRepository.list(text, startDate, endDate, status, BussinessCommon.getUserId(),
				BussinessCommon.getClientId(), pageable);
	}

	@Transactional
	public WordEditor del(Long weId) {
		WordEditor we = valid(weId);
		if (BussinessCommon.getUserId().equals(we.getCreateBy())) {
			notificationService.setActiveByDocIdAndDocType(weId, DocumentTypeEnum.VAN_BAN_SOAN_THAO, false);
			we.setActive(false);
			return wEditorRepository.save(we);
		}
		throw new RestExceptionHandler(Message.DELETE_NOT_ALLOWED);
	}

	public List<WordEditor> addToTask(Long taskId, List<Long> weIds) {
		List<WordEditor> nList = wEditorRepository.findByIds(weIds, BussinessCommon.getClientId());
		List<WordEditor> oList = getByTaskId(taskId);
		List<WordEditor> all = new ArrayList<>();
		all.addAll(nList);
		all.addAll(oList);

		all.forEach(i -> {
			// add
			if (nList.contains(i) && !oList.contains(i)) {
				i.setTaskId(taskId);
			}

			// del
			if (!nList.contains(i) && oList.contains(i)) {
				i.setTaskId(null);
			}
		});

		return wEditorRepository.saveAll(all);
	}

	public List<WordEditor> getList() {
		return wEditorRepository.findByIdss(BussinessCommon.getClientId(), BussinessCommon.getUserId());
	}

	public List<WordEditor> getByTaskId(Long taskId) {
		return wEditorRepository.findByTaskIdAndClientIdAndActiveTrue(taskId, BussinessCommon.getClientId());
	}

	public void removeTaskId(Long taskId) {
		List<WordEditor> oList = getByTaskId(taskId);
		oList.forEach(i -> i.setTaskId(null));
		wEditorRepository.saveAll(oList);
	}

	public List<WordEditorProcess> findByWeId(Long weId) {
		return wEditorProcessRepository.findByWeIdAndClientIdAndActiveTrue(weId, BussinessCommon.getClientId());
	}

	public List<TrackingSendReceiveDto> tracking(Long weId) {
		List<WordEditorProcess> all = findByWeId(weId);
		List<TrackingReceiveDocDto> rList = new ArrayList<>();
		List<TrackingSendReceiveDto> sList = new ArrayList<>();

		if (BussinessCommon.isEmptyList(all))
			return new ArrayList<>();

		List<WordEditorProcess> transfer = setTransfer(all);
		TrackingReceiveDocDto receive = null;
		TrackingSendReceiveDto send = null;
		List<TaskAttachment> attachments = null;
		sList.add(set(send, all.get(0)));
		for (WordEditorProcess i : transfer) {
			send = new TrackingSendReceiveDto(i, false);
			attachments = attService.findByObjId(i.getId(), Constant.TYPE_WORD_EDITOR, BussinessCommon.getClientId(),
					true);
			send.setAttachments(attachments);

			int no = 0;
			rList = new ArrayList<>();
			for (WordEditorProcess j : all) {
				if (j.getFrUser().equals(i.getFrUser()) && (j.getStep().equals(i.getStep()))) {
					receive = new TrackingReceiveDocDto(j, no++);
					rList.add(receive);
				}
			}

			send.setReceiveList(rList);
			sList.add(send);
		}
		return sList;
	}

	public List<WordEditorProcess> setTransfer(List<WordEditorProcess> all) {
		List<WordEditorProcess> transfer = new ArrayList<>();
		Map<PDocDto, WordEditorProcess> map = new HashMap<>();
		all.forEach(i -> {
			PDocDto key = new PDocDto(i.getFrUserId(), i.getStep());
			if (!i.getToUser().equals(i.getFrUser())) {
				map.put(key, i);
			}
		});
		map.forEach((k, v) -> transfer.add(v));
		transfer.sort(Comparator.comparing(WordEditorProcess::getStep));
		return transfer;
	}

	private TrackingSendReceiveDto set(TrackingSendReceiveDto send, WordEditorProcess first) {
		send = new TrackingSendReceiveDto(first, true);
		List<TaskAttachment> attachments = attService.findByObjId(first.getId(), Constant.TYPE_WORD_EDITOR,
				BussinessCommon.getClientId(), true);
		send.setAttachments(attachments);
		return send;
	}

	public List<Comment> comment(Long weId) {
		valid(weId);
		List<Comment> cmts = commentRepository.findByObjIdAndObjTypeAndClientIdAndActiveTrue(weId,
				DocumentTypeEnum.VAN_BAN_SOAN_THAO, BussinessCommon.getClientId());
		List<Long> cmtIds = cmts.stream().map(Comment::getId).collect(Collectors.toList());

		List<TaskAttachment> attList = attService.findByCommentId(cmtIds, Constant.TYPE_WORD_EDITOR_CMT,
				BussinessCommon.getClientId(), true);
		if (!attList.isEmpty()) {
			cmts.forEach(i -> {
				i.setAttachments(attService.getAttachsByCmtId(attList, i.getId()));
			});
		}

		return cmts;
	}

	public void canRead(Long userId, Long weId) {
		WordEditor we = valid(weId);
		boolean thread = wEditorProcessRepository.isUserInThread(weId, userId, BussinessCommon.getClientId());
		boolean taskRelated = taskExecuteService.userInTask(userId, we.getTaskId());
		if (!thread && !taskRelated)
			throw new RestExceptionHandler(Message.NO_PROCESS_DOC);
	}
	
	public List<ReportDocByTypeDto> report() {
		return wEditorProcessRepository.report(BussinessCommon.getUserId(), BussinessCommon.getClientId());
	}
}
