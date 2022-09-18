use std::sync::{atomic::{AtomicI64, Ordering}, mpsc::{self, TrySendError}, Arc};

pub struct TimingSender {
    time: Arc<AtomicI64>,
    notify: mpsc::SyncSender<()>,
}

impl TimingSender {
    pub fn update(&self, time: i64) -> bool {
        self.time.store(time, Ordering::Relaxed);
        match self.notify.try_send(()) {
            Err(TrySendError::Disconnected(())) => false,
            _ => true,
        }
    }
}

pub struct TimingReceiver {
    time: Arc<AtomicI64>,
    wait: mpsc::Receiver<()>,
}

impl TimingReceiver {
    pub fn get(&self) -> i64 {
        self.time.load(Ordering::Relaxed)
    }

    pub fn wait(&self) {
        self.wait.recv().unwrap()
    }
}

pub fn make_timing(initial: i64) -> (TimingSender, TimingReceiver) {
    let (notify, wait) = mpsc::sync_channel(1);
    let time = Arc::new(AtomicI64::new(initial));
    (
        TimingSender {
            time: time.clone(),
            notify,
        },
        TimingReceiver { time, wait },
    )
}
