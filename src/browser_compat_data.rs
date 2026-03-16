use std::{cmp::Ordering, collections::HashMap, sync::LazyLock};

use derive_more::Display;
use itertools::{EitherOrBoth, Itertools};
use serde::Deserialize;
use serde_with::{OneOrMany, serde_as};

const DATA_STR: &str =
    include_str!("../browser-compat-data/node_modules/@mdn/browser-compat-data/data.json");

pub static DATA: LazyLock<Data> = LazyLock::new(Data::new);

#[derive(Debug, Deserialize)]
struct BrowserCompatData {
    javascript: JavascriptData,
}

#[derive(Debug, Deserialize)]
struct JavascriptData {
    __compat: Option<CompatData>,
    #[serde(flatten)]
    path: HashMap<String, JavascriptData>,
}

#[derive(Debug, Deserialize)]
pub struct CompatData {
    pub support: Support,
}

#[serde_as]
#[derive(Debug, Deserialize)]
pub struct Support {
    #[serde_as(as = "OneOrMany<_>")]
    pub chrome: Vec<BrowserSupport>,
}

#[derive(Debug, Deserialize)]
pub struct BrowserSupport {
    pub version_added: VersionAdded,
    #[serde(default)]
    pub partial_implementation: bool,
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
pub enum VersionAdded {
    Version(Version),
    NotAdded(NotAdded),
}

#[derive(Debug, Display, Deserialize, PartialEq, Eq)]
#[repr(transparent)]
pub struct Version(String);

impl Version {
    fn components(&self) -> impl Iterator<Item = u64> {
        let s = self.0.strip_prefix('≤').unwrap_or(&self.0);
        s.split('.').map(|s| s.parse().unwrap())
    }
}

impl Ord for Version {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        for c in self.components().zip_longest(other.components()) {
            match c {
                EitherOrBoth::Both(a, b) => {
                    let v = a.cmp(&b);
                    if v != Ordering::Equal {
                        return v;
                    }
                }
                EitherOrBoth::Left(_) => return Ordering::Greater,
                EitherOrBoth::Right(_) => return Ordering::Less,
            }
        }
        Ordering::Equal
    }
}

impl PartialOrd for Version {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Debug, Deserialize)]
#[serde(try_from = "bool")]
pub struct NotAdded;

impl TryFrom<bool> for NotAdded {
    type Error = &'static str;

    fn try_from(value: bool) -> Result<Self, Self::Error> {
        if value {
            Err("`true` value used for `version_added`")
        } else {
            Ok(Self)
        }
    }
}

pub struct Data {
    data: BrowserCompatData,
}

impl Data {
    fn new() -> Self {
        fn aux(data: &JavascriptData) {
            if let Some(compat) = &data.__compat {
                let n = compat.support.chrome.len();
                assert!(1 <= n);
                assert!(n <= 2);
            }
            for v in data.path.values() {
                aux(v);
            }
        }

        let data: BrowserCompatData = serde_json::from_str(DATA_STR).unwrap();

        aux(&data.javascript);

        Self { data }
    }

    #[must_use]
    pub fn compat_data<'a>(&'a self, js_feature: &[&'static str]) -> &'a CompatData {
        fn aux<'a>(data: &'a JavascriptData, path: &[&'static str]) -> Option<&'a CompatData> {
            match path.split_first() {
                None => data.__compat.as_ref(),
                Some((first, rest)) => Some(aux(data.path.get(*first)?, rest)?),
            }
        }

        let Some(v) = aux(&self.data.javascript, js_feature) else {
            panic!("No compat data found for feature: {js_feature:?}");
        };
        v
    }
}
